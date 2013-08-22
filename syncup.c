// header {{{1
/*
  Copyright (C) 2005 Adam Thomason <athomasonx@users.sourceforge.net>

  http://xmms-syncup.sourceforge.net

  Based on xmms-netamp: http://axelbrown.com/xmms/

  Copyright (C) 2002 Axel Brown <xmms-netamp@thunderlizard.net>

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; either version 2
  of the License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/
// }}}1

// includes {{{1
#include <gtk/gtk.h>
#include <netdb.h>
#include <math.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/poll.h>
#include <sys/wait.h>
#include <arpa/inet.h>
#include <asm/errno.h>
#include <errno.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <setjmp.h>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>
#include <pthread.h>
#include <glib.h>
#include <string.h>
#include "xmmsctrl.h"
#include "configfile.h"
#include "plugin.h"

#include "config.h"
// }}}1

// decls {{{1

// some tasty constants
const int MS_PER_S = 1000;
const int US_PER_MS = 1000;
const int NS_PER_MS = 1000000;

#define DEBUG 0
#define TEMP_BUFFER_SIZE (1024)

// config variables
char* remoteHost = NULL;
int remotePort = 8335;
int sleepTime = 950;
int lowerTolerance = -40;
int upperTolerance = 15;

// state
int syncupEnabled = 1;
int childPID = -1;
gboolean needPlaylist = FALSE;

void syncup_init( void );
void syncup_about( void );
void syncup_config( void );
void syncup_cleanup( void );
void save_cfgfile( gint xmms_session );
void save_playlist( gint xmms_session );

void write_config( gpointer data );
void connectionHandler( int sock );
void processServerInfo( char *input );
void syncTo( int serverTime );

GtkWidget *conf_dialog;
GtkWidget *port_field;
GtkWidget *hostentry;
GtkWidget *tolerance_field;
// }}}1

// window stuff {{{1
void
syncup_about( void )
{
    GtkWindow *about_box = ( GtkWindow * ) gtk_window_new( GTK_WINDOW_TOPLEVEL );
    GtkVBox *container = ( GtkVBox * ) gtk_vbox_new( FALSE, 10 );
    GtkLabel *copyright = ( GtkLabel * ) gtk_label_new( "(c) 2005 Adam Thomason" );
    GtkLabel *email = ( GtkLabel * ) gtk_label_new( "athomasonx@users.sourceforge.net" );
    GtkLabel *url = ( GtkLabel * ) gtk_label_new( "http://xmms-syncup.sourceforge.net" );

    gtk_container_set_border_width( GTK_CONTAINER( container ), 10 );
    gtk_window_set_title( about_box, "xmms-syncup" );

    gtk_container_add( GTK_CONTAINER( container ), GTK_WIDGET( copyright ) );
    gtk_container_add( GTK_CONTAINER( container ), GTK_WIDGET( email ) );
    gtk_container_add( GTK_CONTAINER( container ), GTK_WIDGET( url ) );
    gtk_container_add( GTK_CONTAINER( about_box ), GTK_WIDGET( container ) );

    gtk_widget_show( GTK_WIDGET( copyright ) );
    gtk_widget_show( GTK_WIDGET( container ) );
    gtk_widget_show( GTK_WIDGET( url ) );
    gtk_widget_show( GTK_WIDGET( email ) );
    gtk_widget_show( GTK_WIDGET( about_box ) );
}

void
syncup_config_ok( GtkWidget *wid, gpointer data )
{
    write_config( data );
    gtk_widget_destroy ( conf_dialog );
    conf_dialog = NULL;
}

void
syncup_config( void )
{
    GtkWidget *servoptframe, *table;
    GtkWidget *portlabel;
    GtkObject *portentry;
    GtkWidget *hostlabel;
    GtkWidget *bigbox, *buttonbox;
    GtkWidget *ok_button, *apply_button, *cancel_button;

    if ( conf_dialog )
        return;

    conf_dialog = gtk_window_new ( GTK_WINDOW_DIALOG );

    gtk_window_set_title ( GTK_WINDOW ( conf_dialog ), ( "xmms-syncup config" ) );
    gtk_window_set_policy ( GTK_WINDOW ( conf_dialog ), FALSE, FALSE, FALSE );
    gtk_window_set_position ( GTK_WINDOW ( conf_dialog ), GTK_WIN_POS_MOUSE );

    gtk_container_set_border_width ( GTK_CONTAINER ( conf_dialog ), 5 );

    gtk_signal_connect ( GTK_OBJECT ( conf_dialog ), "destroy", GTK_SIGNAL_FUNC ( gtk_widget_destroyed ), &conf_dialog );

    bigbox = gtk_vbox_new ( FALSE, 5 );
    gtk_container_add ( GTK_CONTAINER ( GTK_WINDOW ( conf_dialog ) ), bigbox );

    // ------------------------------

    servoptframe = gtk_frame_new ( "Server options:" );
    gtk_container_add ( GTK_CONTAINER ( bigbox ), servoptframe );

    table = gtk_table_new( 2, 2, FALSE );
    gtk_container_set_border_width( GTK_CONTAINER( table ), 5 );
    gtk_container_add( GTK_CONTAINER( servoptframe ), table );
    gtk_table_set_row_spacings( GTK_TABLE( table ), 5 );
    gtk_table_set_col_spacings( GTK_TABLE( table ), 5 );

    portlabel = gtk_label_new( "Port: " );
    gtk_misc_set_alignment( GTK_MISC( portlabel ), 1.0, 0.5 );
    gtk_table_attach_defaults( GTK_TABLE( table ), portlabel, 0, 1, 0, 1 );
    gtk_widget_show( portlabel );

    portentry = gtk_adjustment_new ( remotePort, 1, 256 * 256, 1, 1, 1 );
    port_field = gtk_spin_button_new ( GTK_ADJUSTMENT ( portentry ), 1.0, 0 );
    gtk_table_attach_defaults( GTK_TABLE( table ), port_field, 1, 2, 0, 1 );
    gtk_widget_show( port_field );

    hostlabel = gtk_label_new( "Host: " );
    gtk_misc_set_alignment( GTK_MISC( hostlabel ), 1.0, 0.5 );
    gtk_table_attach_defaults( GTK_TABLE( table ), hostlabel, 0, 1, 1, 2 );
    gtk_widget_show( hostlabel );

    hostentry = gtk_entry_new( );
    gtk_widget_set_usize( hostentry, 200, -1 );
    if ( remoteHost )
        gtk_entry_set_text( GTK_ENTRY( hostentry ), remoteHost );
    gtk_table_attach_defaults( GTK_TABLE( table ), hostentry, 1, 2, 1, 2 );
    gtk_widget_show( hostentry );

    // ------------------------------

    buttonbox = gtk_hbutton_box_new ( );
    gtk_button_box_set_layout ( GTK_BUTTON_BOX ( buttonbox ), GTK_BUTTONBOX_END );
    gtk_button_box_set_spacing ( GTK_BUTTON_BOX ( buttonbox ), 5 );
    gtk_box_pack_start ( GTK_BOX ( bigbox ), buttonbox, FALSE, FALSE, 0 );

    ok_button = gtk_button_new_with_label ( "Ok" );
    apply_button = gtk_button_new_with_label ( "Apply" );
    cancel_button = gtk_button_new_with_label ( "Cancel" );

    gtk_signal_connect_object ( GTK_OBJECT ( cancel_button ), "clicked", GTK_SIGNAL_FUNC ( gtk_widget_destroy ), ( gpointer ) conf_dialog );

    gtk_signal_connect_object ( GTK_OBJECT ( apply_button ), "clicked", GTK_SIGNAL_FUNC ( write_config ), NULL );
    gtk_signal_connect_object ( GTK_OBJECT ( ok_button ), "clicked", GTK_SIGNAL_FUNC ( syncup_config_ok ), NULL );

    GTK_WIDGET_SET_FLAGS ( ok_button, GTK_CAN_DEFAULT );
    GTK_WIDGET_SET_FLAGS ( cancel_button, GTK_CAN_DEFAULT );
    GTK_WIDGET_SET_FLAGS ( apply_button, GTK_CAN_DEFAULT );

    gtk_box_pack_start ( GTK_BOX ( buttonbox ), ok_button, TRUE, TRUE, 0 );
    gtk_box_pack_start ( GTK_BOX ( buttonbox ), cancel_button, TRUE, TRUE, 0 );
    gtk_box_pack_start ( GTK_BOX ( buttonbox ), apply_button, TRUE, TRUE, 0 );

    gtk_widget_show_all ( conf_dialog );
}
// }}}1

// util {{{1

/* string copy with len and zero delimit */
char
*strmncpy( char *dest, char *source, unsigned int len )
{
    char *pt;
    if ( ( dest == NULL ) || ( source == NULL ) ) return NULL;
    pt = strncpy( dest, source, len - 1 );
    if ( strlen( source ) + 1 >= len )
    dest[ len - 1 ] = 0;
    return pt;
}

void
write_config( gpointer data )
{
    ConfigFile *config;

    if ( ( config = xmms_cfg_open_default_file( ) ) == NULL )
        config = xmms_cfg_new( );

    if ( remoteHost ) g_free( remoteHost );
    remoteHost = NULL;

    remotePort = gtk_spin_button_get_value_as_int( GTK_SPIN_BUTTON( port_field ) );
    xmms_cfg_write_int ( config, "syncup", "port", remotePort );
    remoteHost = g_strdup( gtk_entry_get_text( GTK_ENTRY( hostentry ) ) );
    xmms_cfg_write_string ( config, "syncup", "host", remoteHost );
    xmms_cfg_write_default_file ( config );

    xmms_cfg_free ( config );
}

void
read_config ( )
{
    ConfigFile *config;

    if ( remoteHost ) g_free( remoteHost );
    remoteHost = NULL;

    if ( ( config = xmms_cfg_open_default_file( ) ) != NULL ) {
        xmms_cfg_read_string( config, "syncup", "host", &remoteHost );
        xmms_cfg_read_int( config, "syncup", "port", &remotePort );
        xmms_cfg_free( config );
    }

    if ( remoteHost == NULL )
        remoteHost = g_strdup_printf( "localhost" );
}

int
ssnprintf( int sock, char *format, ... )
{
    va_list va;
    static char buf[8192];

    va_start( va, format );
    g_vsnprintf( buf, sizeof( buf ), format, va );
    write( sock, buf, strlen( buf ) );

    va_end( va );
    return strlen( buf );
}

static void
childHandler( int signal )
{
    waitpid( -1, NULL, WNOHANG );
}

static void
termHandler( int signal )
{
    syncupEnabled = FALSE;
}

int
timeDiff( struct timeval* t1, struct timeval* t2 )
{
    return
        ( t2->tv_sec - t1->tv_sec ) * MS_PER_S +
        ( t2->tv_usec - t1->tv_usec ) / US_PER_MS;
}

// }}}1

// plugin {{{1

GeneralPlugin syncup =
{
    NULL,
    NULL,
    -1,
    "XMMS-SyncUp plugin " VERSION,
    syncup_init,
    syncup_about,
    syncup_config,
    syncup_cleanup,
};

GeneralPlugin*
get_gplugin_info( void )
{
    return &syncup;
}

void
syncup_cleanup( void )
{
    syncupEnabled = 0;

    if ( remoteHost )
        g_free( remoteHost );
    remoteHost = NULL;

    if ( childPID != -1 ) {
        kill( childPID, SIGTERM );
    }
    signal( SIGCHLD, SIG_DFL );
}

// }}}1

// init {{{1
void
syncup_init( void )
{
    int pid;

    syncupEnabled = 1;

    if ( DEBUG ) printf( "[%d] syncup starting up\n", getpid( ) );

    // lookup tcp protocol info
    struct protoent * tcpInfo = getprotobyname( "tcp" );
    if ( tcpInfo == NULL ) {
        printf( "[%d] Unable to lookup info on TCP\n", getpid( ) );
        exit( 1 );
    }

    read_config( );

    signal( SIGCHLD, childHandler );

    pid = fork( );
    if ( pid == -1 ) {
        printf( "[%d] fork failed\n", getpid( ) );
        exit( 1 );
    }

    if ( pid == 0 ) {
        int errval;
        int sock;
        struct sockaddr_in client;
        signal( SIGTERM, termHandler );

        struct timespec ts;
        ts.tv_sec = sleepTime / MS_PER_S;
        ts.tv_nsec = ( sleepTime % MS_PER_S ) * NS_PER_MS;

        while ( syncupEnabled ) {
            if ( nanosleep( &ts, 0 ) < 0 && !syncupEnabled ) {
                break;
            }

            // create the main socket
            if ( ( sock = socket( AF_INET, SOCK_STREAM, tcpInfo->p_proto ) ) < 0 ) {
                printf( "[%d] socket failed!\n", getpid( ) );
                continue;
            }

            bzero( &client, sizeof( client ) );

            struct hostent* host = gethostbyname( remoteHost );
            if ( !host || !host->h_addr_list ) {
                printf( "[%d] Could not resolve %s", getpid( ), remoteHost );
                syncupEnabled = 0;
            }

            bcopy( host->h_addr_list[0], &( client.sin_addr.s_addr ), host->h_length );
            client.sin_family = AF_INET;
            client.sin_port = htons( remotePort );

            // connect to master
            errval = connect( sock, ( struct sockaddr * ) &client, sizeof( client ) );
            if ( errval == -1 ) {
                char connecterr[128];
                sprintf( connecterr, "[%d] connect to port %d failed: ", getpid( ), remotePort );
                switch( errno ) {
                    case EBADF:     strcat( connecterr, "not a valid socket\n" ); break;
                    case EINVAL:    strcat( connecterr, "socket already bound\n" ); break;
                    case EACCES:    strcat( connecterr, "permission denied\n" ); break;
                    case ENOTSOCK:    strcat( connecterr, "descriptor not a socket\n" ); break;
                    case EADDRINUSE:strcat( connecterr, "address in use\n" ); break;
                    default: sprintf( connecterr + strlen( connecterr ), "unknown error(%d)\n", errno ); break;
                }
                printf( connecterr );
                continue;
            }

            char peerip[16];
            strmncpy( peerip, inet_ntoa( client.sin_addr ), sizeof( peerip ) );

            ssnprintf( sock, "GET /backend/?get=%s\n\n", needPlaylist ? "playlist" : "all" );
            connectionHandler( sock );

            //printf( "[%d] Disconnected from %s\n", getpid( ), peerip );
            close( sock );
        }

        _exit( 0 );
    }
    else {
        childPID = pid;
    }
}
// }}}1

// response handler {{{1
void
connectionHandler( int sock )
{
    int errval = 0;
    char* inputBuffer;
    struct pollfd clientWait;

    int inputBufferSize = 1024;

    clientWait.fd = sock;
    clientWait.events = POLLIN;

    inputBuffer = malloc( sizeof( char ) * inputBufferSize );
    if ( inputBuffer == NULL ) {
        printf( "[%d] Could not allocate enough memory (%dB) for server reply\n", getpid( ), inputBufferSize );
        return;
    }
    memset( inputBuffer, 0, inputBufferSize );

    char* inputBufferCursor = inputBuffer;
    int totalBytesRead = 0;
    for ( ; ; ) {
        int bytesToRead = inputBufferSize - totalBytesRead;

        // especially with the playlist, server may take a while to reply
        errval = poll( &clientWait, 1, 10000 );
        if ( errval < 0 ) {
            printf( "[%d] error on socket\n", getpid( ) );
            free( inputBuffer );
            return;
        }
        if ( errval == 0 ) {
            printf( "[%d] server response timed out\n", getpid( ) );
            free( inputBuffer );
            return;
        }

        int bytesRead = read( sock, inputBufferCursor, bytesToRead );
        totalBytesRead += bytesRead;
        if ( DEBUG > 1 ) printf( "[%d] req=%d got=%d total=%d\n", getpid( ), bytesToRead, bytesRead, totalBytesRead );

        if ( bytesRead < 0 ) {
            perror( "Socket read failed" );
            free( inputBuffer );
            return;
        }
        if ( !bytesRead ) {
            // eof
            break;
        }

        // more data waiting; realloc what me might need and read more
        int extraAllocation = inputBufferSize;
        if ( bytesRead < bytesToRead ) {
            // last read didn't fill the buffer; we guess that we'll need as
            // much additional space as the last read provided
            extraAllocation = bytesRead;
        }
        int newInputBufferSize = inputBufferSize + extraAllocation;

        if ( DEBUG ) printf( "[%d] reallocing %d bytes\n", getpid( ), newInputBufferSize );
        char* newInputBuffer = realloc( inputBuffer, newInputBufferSize );
        if ( newInputBuffer == NULL ) {
            printf( "[%d] Could not allocate enough memory (%dB) for server reply\n", getpid( ), newInputBufferSize );
            free( inputBuffer );
            return;
        }

        // copy old buffer to new
        //memset( newInputBuffer, 0, newInputBufferSize );
        //memcpy( newInputBuffer, inputBuffer, inputBufferSize );
        //free( inputBuffer );

        inputBuffer = newInputBuffer;
        inputBufferSize = newInputBufferSize;
        inputBufferCursor = inputBuffer + totalBytesRead * sizeof( char );
    }

    if ( DEBUG ) printf( "[%d] read %d bytes from server\n", getpid( ), totalBytesRead );

    processServerInfo( inputBuffer );

    free( inputBuffer );
}

void
processServerInfo( char *input )
{
    char* inputEnd = input + sizeof( char ) * strlen( input );

    if ( !needPlaylist ) {
        char* keyStart;
        char* keyEnd;
        char* valueStart;
        char* valueEnd;
        static char key[ TEMP_BUFFER_SIZE ], value[ TEMP_BUFFER_SIZE ];
        int remote_playlist_pos = 0, remote_output_time, remote_playlist_length;
        int dontSeek = 0;

        if ( !xmms_remote_is_playing( syncup.xmms_session ) ) {
            dontSeek = 1;
        }

        keyStart = input;
        while ( keyStart < inputEnd ) {
            // extract key
            keyEnd = strchr( keyStart, '=' );
            if ( !keyEnd ) {
                printf( "[%d] error: server response ended unexpectedly\n", getpid( ) );
                break;
            }
            keyEnd--;

            if ( keyEnd - keyStart + 1 > TEMP_BUFFER_SIZE ) {
                printf( "[%d] error: server response exceeded maximum length\n", getpid( ) );
                return;
            }
            strncpy( key, keyStart, ( keyEnd - keyStart + 1 ) / sizeof( char ) );
            key[ keyEnd - keyStart + 1 ] = 0;

            valueStart = keyEnd + 2;

            // extract value
            valueEnd = strchr( valueStart, '\n' );
            if ( !valueEnd ) {
                printf( "[%d] error: server response ended unexpectedly\n", getpid( ) );
                break;
            }
            valueEnd--;

            if ( valueEnd - valueStart + 1 > TEMP_BUFFER_SIZE ) {
                printf( "[%d] error: server response exceeded maximum length\n", getpid( ) );
                break;
            }
            strncpy( value, valueStart, ( valueEnd - valueStart + 1 ) / sizeof( char ) );
            value[ valueEnd - valueStart + 1 ] = 0;

            //if ( DEBUG ) printf( "[%d]: server response [%s;%s]\n", getpid( ), key, value );

            if ( !strcmp( key, "playlist_pos" ) ) {
                int local_playlist_pos = xmms_remote_get_playlist_pos( syncup.xmms_session );
                remote_playlist_pos = atoi( value );
                if ( remote_playlist_pos != local_playlist_pos ) {
                    // switch songs if it's changed
                    if ( xmms_remote_is_playing( syncup.xmms_session ) ) {
                        printf( "[%d] wrong track (%d), jumping to %d\n", getpid( ),
                            local_playlist_pos, remote_playlist_pos );
                        xmms_remote_set_playlist_pos( syncup.xmms_session, remote_playlist_pos );
                    }
                }
            }
            else if ( !strcmp( key, "is_running" ) || !strcmp( key, "is_playing" ) ) {
                if ( !atoi( value ) )
                    dontSeek = 1;
            }
            else if ( !strcmp( key, "is_paused" ) ) {
                if ( atoi( value ) )
                    dontSeek = 1;
            }
            else if ( !strcmp( key, "output_time" ) ) {
                if ( !dontSeek ) {
                    remote_output_time = atoi( value );
                    syncTo( remote_output_time );
                }
            }
            else if ( !strcmp( key, "playlist_length" ) ) {
                int local_playlist_length = xmms_remote_get_playlist_length( syncup.xmms_session );
                remote_playlist_length = atoi( value );
                if ( remote_playlist_length != local_playlist_length ) {
                    if ( DEBUG ) printf( "[%d] playlist length mismatch (client=%d,server=%d), requesting playlist\n",
                        getpid( ), local_playlist_length, remote_playlist_length );
                    needPlaylist = TRUE;
                }
            }
            else if ( !strcmp( key, "playlist_file" ) ) {
                // we want the _local_ name of the track the _server_ is on
                char* local_playlist_file = xmms_remote_get_playlist_file( syncup.xmms_session, remote_playlist_pos );
                if ( local_playlist_file ) {
                    if ( strcmp( local_playlist_file, value ) ) {
                        if ( DEBUG ) printf( "[%d] title mismatch, requesting playlist\n", getpid( ) );
                        needPlaylist = TRUE;
                    }
                }
            }

            keyStart = valueEnd + 2; // advance past \n to beginning of next field
        }
    }
    else {
        // build a list, one file per line
        GList* list = NULL;

        char* filenameStart;
        char* filenameEnd;
        char* filename;

        //if ( DEBUG ) printf( "[%d] input:{%s}\n", getpid( ), input );

        filenameStart = input;
        while ( filenameStart < inputEnd ) {
            // extract filename: first of three tab-delimited fields
            filenameEnd = strchr( filenameStart, '\t' );
            if ( !filenameEnd ) {
                printf( "[%d] error: server response missing \\t\n", getpid( ) );
                break;
            }
            filenameEnd--;

            if ( filenameEnd - filenameStart + 1 > TEMP_BUFFER_SIZE ) {
                printf( "[%d] error: server response exceeded maximum length\n", getpid( ) );
                break;
            }
            filename = malloc( ( filenameEnd - filenameStart + 2 ) * sizeof( char ) );
            strncpy( filename, filenameStart, ( filenameEnd - filenameStart + 1 ) / sizeof( char ) );
            filename[ filenameEnd - filenameStart + 1 ] = 0;
            list = g_list_append( list, filename );

            // advance past \n to beginning of next field
            filenameStart = strchr( filenameEnd, '\n' );
            if ( !filenameStart ) {
                printf( "[%d] error: server response missing \\n\n", getpid( ) );
                break;
            }
            filenameStart++;
        }

        if ( list ) {
            xmms_remote_playlist_clear( syncup.xmms_session );
            xmms_remote_playlist_add( syncup.xmms_session, list );
        }
        else {
            printf( "[%d] warning: server playlist is empty\n", getpid( ) );
        }

        // free up the list data
        GList* p = list;
        while ( p ) {
            free( ( char* ) p->data );
            p = p->next;
        }
        g_list_free( list );

        needPlaylist = FALSE;
    }
}

// }}}1

// seek logic {{{1
void
syncTo( int serverTime )
{
    int localTime = xmms_remote_get_output_time( syncup.xmms_session );

    int error = serverTime - localTime;

    if ( DEBUG ) printf( "[%d] server=%06d, local=%06d, diff=%6d", getpid( ), serverTime, localTime, error );

    if ( lowerTolerance > error || error > upperTolerance ) {
        // mpg123 seeks to whole MS_PER_S only
        struct timeval presleep, postsleep;
        int requestedSeek;
        struct timespec ts;
        /*
        if ( absError < MS_PER_S && 950 < serverTime % MS_PER_S ) {
            // just before the next MS_PER_S, close enough to seek right there
            requestedSeek = serverTime - serverTime % MS_PER_S + MS_PER_S;
            printf( "serverTime=%d requestedSeek=%d direct\n", serverTime, requestedSeek );
        }
        */

        // wait until just before the next MS_PER_S then seek
        int margin = 25;
        requestedSeek = serverTime - serverTime % MS_PER_S + MS_PER_S;
        int wait = MS_PER_S - margin - serverTime % MS_PER_S - 50;
        if ( wait < 0 ) wait = 0;
        if ( DEBUG ) printf( ", wait=%03d", wait );

        // track time we actually sleep compared to what we want
        ts.tv_sec = wait / MS_PER_S;
        ts.tv_nsec = ( wait % MS_PER_S ) * NS_PER_MS;
        gettimeofday( &presleep, 0 );
        nanosleep( &ts, 0 );
        gettimeofday( &postsleep, 0 );
        int sleepError = timeDiff( &presleep, &postsleep ) - wait;
        if ( DEBUG ) printf( ", error=%03d", sleepError );

        if ( abs( sleepError ) < 10 ) {
            // only do the seek if we slept nearly the correct amount of time
            xmms_remote_jump_to_time( syncup.xmms_session, requestedSeek );
            if ( DEBUG ) printf( " => seek to %06d\n", requestedSeek );
        }
        else {
            if ( DEBUG ) printf( " => no seek" );
        }
    }
    else {
        if ( DEBUG ) printf( ", ok" );
    }

    if ( DEBUG ) printf( "\n" );
}

// }}}1

// modeline {{{1
// vim: foldmethod=marker
// }}}1

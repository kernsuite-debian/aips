#define Z_tksrv__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <netdb.h>
#ifndef __linux__
#include <sgtty.h>
#else
#include <bsd/sgtty.h>
#endif
#include <errno.h>
#include <signal.h>

static int num_calls = 0;      /* The number of active calls to
                                  zabors.  Since zabors should not
                                  be called recursively this should
                                  be 0 or 1 */
int domain_type;
char tkdunixdev[108];
#if defined(__STDC__)                     /* only semi-ANSI C        */
   void (*zignal_ ( int, void (*)(int) ))( int ) ;
   extern int kill(pid_t, int);
#else                                     /* non-ANSI functions only */
   void (*zignal_ (/* int, void (*)(int) */))(/* int */) ;
   extern int kill(/* pid_t, int */);
#endif
                                      /* Local defines. */

#define BUFLEN 4096 /* Maximum length of a buffer. */

#if __STDC__
   void ztksrv_(void)
#else
   void ztksrv_( )
#endif
/*--------------------------------------------------------------------*/
/*! Provide Tektronix-like display in X-Windows environment           */
/*# Graphics Plot                                                     */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998                                          */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/* tekserver - Enable a tek window to be used by a remote process.    */
/*                                                                    */
/* Description -                                                      */
/*     This server uses INET domain sockets to communicate with AIPS  */
/*     when doing I/O to a tektool that is running under a seperate   */
/*     process than the AIPS task.  Its job is real simple, pass data */
/*     from AIPS to the tektool and when GIN mode is sent, read the   */
/*     coordinates and send them back to the aips process.            */
/*                                                                    */
/*     The reason we need to do this is that if we just open the      */
/*     device in aips, output works fine but input does not work.     */
/*     Hence the server to handle the work.                           */
/*                                                                    */
/* Environment -                                                      */
/*     There are no special environmental variables or such needed    */
/*     by this routine.  However, a service does need to be entered   */
/*     in the /etc/services file.  The line should look like:         */
/*                                                                    */
/*   tekserv         5009/tcp        TEKSERV         # AIPS TekServer */
/*                                                                    */
/*     If you are using yp/nis, then the appropriate yp maps should   */
/*     be updated.                                                    */
/*                                                                    */
/*     In the AIPS environment however, TEKTK* should be specified    */
/*     as the machine on which tekserver is running, generally the    */
/*     same host.                                                     */
/*                                                                    */
/* History-                                                           */
/*     22Mar89 - Created by Jonathan D. Eisenhamer.                   */
/*      5Apr89 - Converted to use the INET domain. jde                */
/*                                                                    */
/* Bugs-                                                              */
/*    I am sure there are many.  One improvement would be to provided */
/*    a multiple server (i.e. fork a new tektool at every incoming    */
/*    connection).  However, with AIPS this would not make much sense,*/
/*    because AIPS closes and reopens the TEKTK channel for each TEK  */
/*    operation (due because the seperate tasks must open the tek     */
/*    device to use it).                                              */
/*                                                                    */
/* SGI version - remove berkeley sgttyb stuff                         */
/* Alpha version copied directly from SGI version.  Can't do ioctl's  */
/* because no permission.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Declarations */
  struct sockaddr_in sock_name;         /* Socket pathname. */
  struct sockaddr_in in_addr;           /* Incoming address. */
  struct sockaddr_un server_un;
  struct sockaddr_un from_un;

  struct servent *sp;                   /* Internet service info.     */

  char buf[BUFLEN];                     /* Character buffer.          */
  char *name;                           /* Pointer to socket name.    */
  char *device;                         /* Pointer to translated dev  */
  char catalog[1024];                   /* image catalog */
  int isvirgin = 1;                     /* Init needed ? */
  int s;                                /* Socket file descriptor.    */
  int in_addr_len;                      /* Incoming address length.   */
  int snew;                             /* New socket connection. */
  int buflen;                           /* Length of incoming data. */
  int fd;                               /* File descr.tektool window  */
  int i, j;                             /* Generic integers. */
  int ic;                               /* Input byte count. */
  int bytes_to_go, nbytes;              /* Bytes to read yet. */
  int accept_num;
  int len;
#if defined(__STDC__)                     /* only semi-ANSI C        */
   void ztkabo_ (int) ;
#else                                     /* non-ANSI functions only */
   void ztkabo_ (/* int */) ;
#endif                           /* __STDC__        */
/*--------------------------------------------------------------------*/
                                        /* set up shut down           */
   zignal_ (SIGINT, ztkabo_);           /* interrupt                  */
   zignal_ (SIGHUP, ztkabo_);           /* hangup                     */
   zignal_ (SIGQUIT, ztkabo_);          /* quit (ignore)              */
   zignal_ (SIGILL, ztkabo_);           /* illegal instruction        */
#ifdef SIGTRAP
   zignal_ (SIGTRAP, ztkabo_);          /* trace trap                 */
#endif
#ifdef SIGABRT
   zignal_ (SIGABRT, ztkabo_);          /* abort (IOT) instruction    */
#else
   zignal_ (SIGIOT, ztkabo_);           /* abort (IOT) instruction    */
#endif
#ifdef SIGEMT
   zignal_ (SIGEMT, ztkabo_);           /* EMT instruction            */
#endif
   zignal_ (SIGFPE, ztkabo_);           /* floating-point exception   */
   zignal_ (SIGKILL, SIG_DFL);          /* kill: can't catch or ignore*/
#ifdef SIGBUS
   zignal_ (SIGBUS, ztkabo_);           /* bus error                  */
#endif
   zignal_ (SIGSEGV, ztkabo_);          /* segmentation violation     */
#ifdef SIGSYS
   zignal_ (SIGSYS, ztkabo_);           /* bad argument to system call*/
#endif
   zignal_ (SIGPIPE, ztkabo_);          /* write on a pipe with no one*/
                                        /*    to read it              */
   zignal_ (SIGTERM, ztkabo_);          /* software termination       */
                                        /*    (e.g., "ABORTASK")      */
   zignal_ (SIGUSR1, ztkabo_);          /* user defined signal 1      */
   zignal_ (SIGUSR2, ztkabo_);          /* user defined signal 2      */

   zignal_ (SIGALRM, ztkabo_);          /* alarm clock                */

   if ((device = getenv(getenv("TVDEV"))) == NULL) {
                                        /* undefined                  */
      fprintf (stderr, "MSGSERVER: NO SUCH DEVICE = TVDEV\n");
      sleep(5);
      exit(errno);
      }
                                        /* open socket, INET domain   */
   if (strncmp(device,"sssin",5) == 0) {
      domain_type = 0;
                                      /* Get the service information. */
      if ( ( sp = getservbyname( "tekserv", "tcp" ) ) == 0 ) {
         perror( "tekserver: could not get service" );
         sleep(5);
         exit( errno );
         }

                                                /* Create the socket. */
      if ( ( s = socket( AF_INET, SOCK_STREAM, 0 ) ) == -1 ) {
         perror( "tekserver: unable to create socket" );
         sleep(5);
         exit( errno );
         }
      fprintf (stderr, "Enter TEKSERV.C\n");

                                            /* Bind to a socket name. */
      sock_name.sin_family = AF_INET;
      sock_name.sin_addr.s_addr = INADDR_ANY;
      sock_name.sin_port = sp->s_port;
      if ( ( bind( s, (struct sockaddr *)&sock_name,
         sizeof( sock_name ) ) ) == -1 ) {
         perror( "tekserver: unable to bind to socket" );
         sleep(5);
         exit( errno );
         }
      }
                                         /* UNIX domain */
   else {
      domain_type = 1;
      server_un.sun_family = AF_UNIX;
      if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
         perror ("MakeLink: socket (UNIX)");
         sleep(5);
         exit(errno);
         }
      if ((device = getenv(getenv("TKDEV"))) == NULL) {
                                        /* undefined                  */
         fprintf (stderr, "MSGSERVER: NO SUCH DEVICE = TKDEV\n");
         sleep(5);
         exit(errno);
         }
                                         /* Otherwise, open socket    */
      unlink (device);                   /* first unlink if it exists */
      strcpy (server_un.sun_path, device);
      strcpy (tkdunixdev, device);
      if (bind (s, (struct sockaddr *)&server_un,
         strlen(server_un.sun_path) + 2) < 0) {
         perror ("MakeLink: bind error (UNIX)");
         sleep(5);
         exit(errno);
         }
      }
                                               /* clear image catalog */
   for (i = 0; i < 1024; i++) catalog[i] = '\0' ;

                 /* Since we are the server, listen for a connection. */
   if ( listen( s, 5 ) == -1 ) {
      perror( "tekserver: listening on socket failed" );
      sleep(5);
      exit( errno );
      }

                  /* Loop forever waiting for an incoming connection. */
   while ( 1 ) {

                                      /* Get the incoming connection. */
      if (domain_type == 0 ) {
         in_addr_len = sizeof (in_addr);
         if ((snew = accept (s, (struct sockaddr *) &in_addr,
            &in_addr_len)) == -1 ) {
            perror("tekserver: could not accept on the socket");
            sleep (5);
            exit (errno);
            }
         }
      else {               /* UNIX_DOMAIN                */
	 len = sizeof(from_un);
	 if ((snew = accept (s, (struct sockaddr *) &from_un, &len)) < 0) {
            perror("TVSERVER: could not accept on the socket");
            sleep (5);
            exit (errno);
            }
         }

                                         /* Open the tektronix device */
      if ((fd = open (ttyname(1), O_RDWR)) == -1) {
         perror( "tekserver: Unable to open tty" );
         sleep(5);
         exit( errno );
         }

      while ((ic = read (snew, buf, BUFLEN)) > 0) {

         if (isvirgin) {
            isvirgin = 0;
            if (write (fd, "\033[?38h", 6) != 6) {
               perror ("tekserver: Bad init to the terminal" );
               sleep (5);
               exit (errno);
               }
            }

                                     /* trap catalog operations       */
         j = 0;
         if ((buf[ic-1] == '\001') && (buf[ic-2] == buf[ic-1])) {
            j = 1;
            ic = ic - 2;
            }
         else if ((buf[ic-1] == '\002') && (buf[ic-2] == buf[ic-1])) {
            if (write (snew, catalog, 1024) != 1024) {
               perror ("tekserver: unable to write catalog to client");
               }
            ic = ic - 2;
            }

                                     /* Write it out to the terminal. */
         if (ic > 0) {
            if (write (fd, buf, ic) != ic ) {
               perror ("tekserver: Bad write to the terminal");
               sleep (5);
               exit (errno);
               }

            /* Check to see if this is entering GIN mode.  If so, then
                                              read from the terminal. */
            if ((buf[ic-2] == '\033') && (buf[ic-1] == '\032')) {
               bytes_to_go = 5;
               while (bytes_to_go) {
                  if ((nbytes = read (fd, buf, bytes_to_go)) == -1 ) {
                     perror
                        ("tekserver: unable to read coordinates back");
                     sleep (5);
                     exit (errno);
                     }
                  bytes_to_go -= nbytes;
                  }

               if (write (snew, buf, 5) != 5 ) {
                  perror( "tekserver: Unable to write back to client" );
                  sleep (5);
                  exit (errno);
                  }
               }
            }
         if (j == 1) {
            if (write (snew, buf, 2) != 2) {
               perror ("tekserver: unable to write ACK to client");
               }
            else {
               if ((ic = read (snew, buf, BUFLEN)) == 1024) {
                  for (i = 0; i < 1024; i++) catalog[i] = buf[i];
                  if (write (snew, buf, 1024) != 1024) {
                     perror ("tekserver: unable to write catalog to client");
                     }
                  }
               else {
                  fprintf (stderr, "catalog read wrong length %d\n", ic);
                  }
               }
            }
         } /* while ( ( ic = read... */

                                             /* Close the connection. */
      if ( close( snew ) == -1 ) {
         perror( "tekserver: could not close connection" );
         sleep(5);
         exit( errno );
         }

                                       /* Close the tektronix device. */
      if ( close( fd ) == -1 ) {
         perror( "tekserver: could not close tek device" );
         sleep(5);
         exit( errno );
         }
      } /* while ( 1 ) */

}

#if __STDC__
   void ztkabo_(int isig)
#else
   void ztkabo_(isig)
   int isig;
#endif
/*--------------------------------------------------------------------*/
{
   int i;
   pid_t  pid;
/*--------------------------------------------------------------------*/
                                        /* Die immediately if this is */
                                        /* a recursive call           */
   if (num_calls != 0) {
                                        /* Get process id for kill(). */
      pid = getpid ();
                                        /* Reinstate default action   */
                                        /* for illegal instructions.  */
      zignal_ (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
      kill (pid, SIGILL);
      }
   else {
      num_calls++;
                                        /* Signal number held in low  */
                                        /* order 7 bits.              */
      i = isig & 0177;
                                        /* Report signal received.    */
      fprintf (stderr, "ZTKABO: signal %d received\n", i);
      if (domain_type == 1) unlink (tkdunixdev);
      sleep(4);
                                        /* Just exit on some errors.  */
      i = isig & 0177;
      if (i < 4 || i == 15) {
         exit(1);
         }
                                        /* On others, abort.          */
      else {
                                        /* Get process id for kill(). */
         pid = getpid ();
                                        /* Reinstate default action   */
                                        /* for illegal instructions.  */
         zignal_ (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
         kill (pid, SIGILL);
         }

      num_calls--;
      }

   return;
}

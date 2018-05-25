#define Z_tvsrv__
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
#if !defined(__linux__) || defined(HAVE_LINUX_GLIBC)
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
char tvlunixdev[108];
int s;                                 /* Socket file descriptor.    */

#if defined(__STDC__)                     /* only semi-ANSI C        */
   void (*zignal_ ( int, void (*)(int) ))( int ) ;
   extern int kill(pid_t, int);
#else                                     /* non-ANSI functions only */
   void (*zignal_ (/* int, void (*)(int) */))(/* int */) ;
   extern int kill(/* pid_t, int */);
#endif

#define BUFLEN 20                       /* Maximum length of buffer   */
#define OOPSDELAY 4                     /* Time to sleep after booboo */


#if __STDC__
   void ztvsrv_(void)
#else
   void ztvsrv_( )
#endif
/*--------------------------------------------------------------------*/
/*! TV lock server                                                    */
/*# TV                                                                */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997-1998                                               */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;  Eric W. Greisen                                                  */
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
/*  Receives inet socket AIPS TV requests from clients.               */
/*  Cloned from TEKSERVER                                             */
/*                                                                    */
/* Description -                                                      */
/*     This server uses INET domain sockets to communicate with AIPS  */
/*     when doing I/O to a window tool that is running under a        */
/*     separate process than the AIPS task.  Its job is simply to     */
/*     display the received message and the host which sent it in its */
/*     window.                                                        */
/*                                                                    */
/* Environment -                                                      */
/*     There are no special environmental variables or such needed    */
/*     by this routine.  However, a service does need to be entered   */
/*     in the /etc/services file.  The line should look like:         */
/*                                                                    */
/*        ssslock  502?/tcp    SSSLOCK  # Aips TV lock server         */
/*                                                                    */
/*     If you are using yp, then the appropriate yp/nis maps should   */
/*     be updated.                                                    */
/*--------------------------------------------------------------------*/
{
  struct sockaddr_un server_un;
  struct sockaddr_un from_un;
  struct sockaddr_in sock_name;         /* Socket pathname.           */
  struct sockaddr_in in_addr;           /* Incoming address.          */

  struct servent *sp;                   /* Internet service info.     */
  struct hostent *remhost;              /* remote client entry        */

  char buf[BUFLEN];                     /* Character buffer.          */
  char *name;                           /* Pointer to socket name.    */
  char *device;                         /* Pointer to translated dev  */

  int in_addr_len;                      /* Incoming address length.   */
  int snew;                             /* New socket connection.     */
  int fd;                               /* File descr. of msg window. */
  int i;                                /* Generic integer.           */
  int ic;                               /* Input byte count.          */
  static int nbadh = 0;                 /* error count for bad host   */
  int islocked[36];                     /* lock flag                  */
  int len;

  int finished = 0;                     /* Shutdown requested?        */

#if defined(__STDC__)                     /* only semi-ANSI C        */
   void ztlabo_ (int) ;
#else                                     /* non-ANSI functions only */
   void ztlabo_ (/* int */) ;
#endif                           /* __STDC__        */
/*--------------------------------------------------------------------*/
   for (i=0; i<36; i++) islocked[i] = 0;
   if ((device = getenv(getenv("TVDEV"))) == NULL) {
                                        /* undefined                  */
      fprintf (stderr, "TVSERVER: NO SUCH DEVICE = TVDEV\n");
      sleep (OOPSDELAY);
      exit(errno);
      }
                                        /* set up shut down           */
   zignal_ (SIGINT, SIG_IGN);           /* interrupt CTRL-C ignore    */
   zignal_ (SIGHUP, ztlabo_);           /* hangup                     */
   zignal_ (SIGQUIT, SIG_IGN);          /* quit (ignore)              */
   zignal_ (SIGILL, ztlabo_);           /* illegal instruction        */
#ifdef SIGTRAP
   zignal_ (SIGTRAP, ztlabo_);          /* trace trap                 */
#endif
#ifdef SIGABRT
   zignal_ (SIGABRT, ztlabo_);          /* abort (IOT) instruction    */
#else
   zignal_ (SIGIOT, ztlabo_);           /* abort (IOT) instruction    */
#endif
#ifdef SIGEMT
   zignal_ (SIGEMT, ztlabo_);           /* EMT instruction            */
#endif
   zignal_ (SIGFPE, ztlabo_);           /* floating-point exception   */
   zignal_ (SIGKILL, SIG_DFL);          /* kill: can't catch or ignore*/
#ifdef SIGBUS
   zignal_ (SIGBUS, ztlabo_);           /* bus error                  */
#endif
   zignal_ (SIGSEGV, ztlabo_);          /* segmentation violation     */
#ifdef SIGSYS
   zignal_ (SIGSYS, ztlabo_);           /* bad argument to system call*/
#endif
   zignal_ (SIGPIPE, ztlabo_);          /* write on a pipe with no one*/
                                        /*    to read it              */
   zignal_ (SIGTERM, ztlabo_);          /* software termination       */
                                        /*    (e.g., "ABORTASK")      */
   zignal_ (SIGUSR1, ztlabo_);          /* user defined signal 1      */
   zignal_ (SIGUSR2, ztlabo_);          /* user defined signal 2      */

   zignal_ (SIGALRM, ztlabo_);          /* alarm clock                */
                                        /* open socket, INET domain   */
   if (strncmp(device,"sssin",5) == 0) {
      domain_type = 0;
                                          /* Get service information. */
      if ((sp = getservbyname("ssslock", "tcp")) == 0) {
         perror ("TVSERVER: could not get ssslock service by name");
         fprintf (stderr,
            "TVSERVER: Check your /etc/services or NIS/YP map\n");
         sleep (OOPSDELAY);
         exit(errno);
         }
                                        /* Create the socket. */
      if ((s = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
         perror ("TVSERVER: unable to create inet socket");
         sleep (OOPSDELAY);
         exit (errno);
         }
      fprintf (stderr,
         "TVSERVER: Starting AIPS TV locking, Inet domain\n");
                                        /* Bind to a socket name. */
      sock_name.sin_family = AF_INET;
      sock_name.sin_addr.s_addr = INADDR_ANY;
      sock_name.sin_port = sp->s_port;
      if ((bind(s, (struct sockaddr *)&sock_name,
         sizeof(sock_name))) == -1) {
         perror("TVSERVER: unable to bind to socket");
         finished = 1;
         }
      }
                                         /* UNIX domain */
   else {
      if ((device = getenv(getenv("TVLOK"))) == NULL) {
                                        /* undefined                  */
         fprintf (stderr, "TVSERVER: NO SUCH DEVICE = TVLOK\n");
         sleep (OOPSDELAY);
         exit(errno);
         }
                                         /* Otherwise, open socket    */
      unlink (device);                   /* first unlink if it exists */
      domain_type = 1;
      server_un.sun_family = AF_UNIX;
      if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
         perror ("MakeLink: socket (UNIX)");
         sleep (OOPSDELAY);
         exit(errno);
         }
      fprintf (stderr,
         "TVSERVER: Starting AIPS TV locking, Unix (local) domain\n");
      strcpy (server_un.sun_path, device);
      strcpy (tvlunixdev, device);
      if (bind (s, (struct sockaddr *)&server_un,
         strlen(server_un.sun_path) + 2) < 0) {
         perror ("MakeLink: bind error (UNIX)");
         finished = 1;
         }
      }
                                        /* Since we are the server,   */
                                        /* listen for a connection.   */
   if (! finished) {
      if (listen(s, 5) == -1) {
         perror("TVSERVER: listening on socket failed");
         finished = 1;
         }
      }
                                        /* Handle incoming            */
                                        /* connections until told to  */
                                        /* shut down.                 */
  while (! finished) {
                                        /* Get incoming connection.   */
      if (domain_type == 0) {
         in_addr_len = sizeof(in_addr);
         if ((snew = accept(s, (struct sockaddr *) &in_addr,
            &in_addr_len)) == -1) {
            perror("TVSERVER: could not accept on the socket");
            finished = 1;
            }
         }
      else {               /* UNIX_DOMAIN                */
         len = sizeof(from_un);
         if ((snew = accept (s, (struct sockaddr *) &from_un, &len))
            < 0) {
            perror("TVSERVER: could not accept on the socket");
            finished = 1;
            }
         }
                                        /* read a line from socket    */

      if (! finished) {
         while ((ic = read (snew, buf, BUFLEN)) > 0) {
                                        /* which TV                   */
            if ((buf[0] >= '0') && (buf[0] <= '9'))
               i = buf[0] - '0';
            else if ((buf[0] >= 'A') && (buf[0] <= 'Z'))
               i = buf[0] - 'A';
            else if ((buf[0] >= 'a') && (buf[0] <= 'z'))
               i = buf[0] - 'a';
            else
               i = 1;
            if (buf[1] == 'L') {
               if (islocked[i-1] > 0) {
                  buf[0] = 'N';
                  buf[1] = 'O';
                  }
               else {
                  islocked[i-1] = 1;
                  buf[0] = 'O';
                  buf[1] = 'K';
                  }
               }
            else if (buf[1] == 'D') {
               buf[0] = 'X' ;
               buf[1] = 'X' ;
               }
            else {
               if (islocked[i-1] > 0) {
                  islocked[i-1] = 0;
                  buf[0] = 'O';
                  buf[1] = 'K';
                  }
               else {
                  buf[0] = 'U';
                  buf[1] = 'N';
                  }
               }
            if (write (snew, buf, 2) != 2) {
               perror("TVSERVER: Failed to send 'OK' to client");
               finished = 1;
               }
            if (buf[1] == 'X') {
               fprintf (stderr, "TVSERVER told to shut down by XAS\n");
               finished = 1;
               }
            }
                                        /* Close the connection.      */
         if (close(snew) == -1) {
            perror("TVSERVER: could not close connection");
            finished = 1;
            }
         }
      }
                                        /* Tidy up                     */
   shutdown (s, 0);
   close (s);
   if (domain_type == 1) unlink (tvlunixdev);
   exit (errno);
}                                       /* end of main                */

#if __STDC__
   void ztlabo_(int isig)
#else
   void ztlabo_(isig)
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
      fprintf (stderr, "ZTLABO: signal %d received\n", i);
      shutdown(s,0) ;
      close(s);
      if (domain_type == 1) unlink (tvlunixdev);
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

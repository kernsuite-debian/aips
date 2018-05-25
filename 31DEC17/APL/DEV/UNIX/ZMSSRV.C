#define Z_mssrv__
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
char msgunixdev[108];
int s;                                  /* Socket file descriptor.    */

#if defined(__STDC__)                     /* only semi-ANSI C        */
   void (*zignal_ ( int, void (*)(int) ))( int ) ;
   extern int kill(pid_t, int);
#else                                     /* non-ANSI functions only */
   void (*zignal_ (/* int, void (*)(int) */))(/* int */) ;
   extern int kill(/* pid_t, int */);
#endif

#define BUFLEN 200                      /* Maximum length of buffer   */
#define OOPSDELAY 5                     /* Time to sleep after booboo */

#if __STDC__
   void zmssrv_(void)
#else
   void zmssrv_( )
#endif
/*--------------------------------------------------------------------*/
/*! msgserver -- recipient of inet AIPS messages for message terminal */
/*# Text                                                              */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998, 2005                                    */
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
/*  Receives inet socket AIPS messages from one or more clients.      */
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
/*        msgserv  5020/tcp    MSGSERV  # Aips message server         */
/*                                                                    */
/*     If you are using yp, then the appropriate yp/nis maps should   */
/*     be updated.                                                    */
/*                                                                    */
/*     In the AIPS environment however, TTDEVnn should be specified   */
/*     as the machine on which msgserver is running, generally the    */
/*     same host.                                                     */
/*                                                                    */
/* History-                                                           */
/*     22Mar89 - Created by Jonathan D. Eisenhamer.                   */
/*      5Apr89 - Converted to use the INET domain. jde                */
/*     22Feb93 - cloned from tekserver to msgserver (ppm)             */
/*--------------------------------------------------------------------*/
{
  struct sockaddr_in sock_name;         /* Socket pathname.           */
  struct sockaddr_in in_addr;           /* Incoming address.          */
  struct sockaddr_un server_un;
  struct sockaddr_un from_un;

  struct servent *sp;                   /* Internet service info.     */
  struct hostent *remhost;              /* remote client entry        */

  char buf[BUFLEN];                     /* Character buffer.          */
  char outbuf[BUFLEN+10];               /* Place to put formatted out */
  char *name;                           /* Pointer to socket name.    */
  char rhost[7];                        /* part of remote hostname    */
  char *device;                         /* Pointer to translated dev  */
  char *lochost;

  int in_addr_len;                      /* Incoming address length.   */
  int snew;                             /* New socket connection.     */
  int fd;                               /* File descr. of msg window. */
  int i;                                /* Generic integer.           */
  int ic;                               /* Input byte count.          */
  static int nbadh = 0;                 /* error count for bad host   */
  int len;
  int finished = 0;                     /* Shutdown requested?        */

#if defined(__STDC__)                     /* only semi-ANSI C        */
   void zmsabo_ (int) ;
#else                                     /* non-ANSI functions only */
   void zmsabo_ (/* int */) ;
#endif                           /* __STDC__        */

/*--------------------------------------------------------------------*/
   if ((device = getenv(getenv("TVDEV"))) == NULL) {
                                        /* undefined                  */
      fprintf (stderr, "MSGserver: NO SUCH DEVICE = TVDEV\n");
      sleep (OOPSDELAY);
      exit(errno);
      }
                                        /* set up shut down           */
   zignal_ (SIGINT, zmsabo_);           /* interrupt                  */
   zignal_ (SIGHUP, zmsabo_);           /* hangup                     */
   zignal_ (SIGQUIT, SIG_IGN);          /* quit (ignore)              */
   zignal_ (SIGILL, zmsabo_);           /* illegal instruction        */
#ifdef SIGTRAP
   zignal_ (SIGTRAP, zmsabo_);          /* trace trap                 */
#endif
#ifdef SIGABRT
   zignal_ (SIGABRT, zmsabo_);          /* abort (IOT) instruction    */
#else
   zignal_ (SIGIOT, zmsabo_);           /* abort (IOT) instruction    */
#endif
#ifdef SIGEMT
   zignal_ (SIGEMT, zmsabo_);           /* EMT instruction            */
#endif
   zignal_ (SIGFPE, zmsabo_);           /* floating-point exception   */
   zignal_ (SIGKILL, SIG_DFL);          /* kill: can't catch or ignore*/
#ifdef SIGBUS
   zignal_ (SIGBUS, zmsabo_);           /* bus error                  */
#endif
   zignal_ (SIGSEGV, zmsabo_);          /* segmentation violation     */
#ifdef SIGSYS
   zignal_ (SIGSYS, zmsabo_);           /* bad argument to system call*/
#endif
   zignal_ (SIGPIPE, zmsabo_);          /* write on a pipe with no one*/
                                        /*    to read it              */
   zignal_ (SIGTERM, zmsabo_);          /* software termination       */
                                        /*    (e.g., "ABORTASK")      */
   zignal_ (SIGUSR1, zmsabo_);          /* user defined signal 1      */
   zignal_ (SIGUSR2, zmsabo_);          /* user defined signal 2      */

   zignal_ (SIGALRM, zmsabo_);          /* alarm clock                */
                                        /* open socket, INET domain   */
   if (strncmp(device,"sssin",5) == 0) {
      domain_type = 0;
                                          /* Get service information. */
      if ((sp = getservbyname("msgserv", "tcp")) == 0) {
         perror ("MSGserver: could not get msgserv service by name");
         fprintf (stderr,
            "MSGserver: Check your /etc/services or NIS/YP map\n");
         sleep (OOPSDELAY);
         exit(errno);
         }
                                        /* Create the socket. */
      if ((s = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
         perror ("MSGserver: unable to create inet socket");
         sleep (OOPSDELAY);
         exit (errno);
         }
      fprintf (stderr,
         "MSGserver: Starting AIPS task logging, Inet domain\n\n");
                                        /* Bind to a socket name. */
      sock_name.sin_family = AF_INET;
      sock_name.sin_addr.s_addr = INADDR_ANY;
      sock_name.sin_port = sp->s_port;
      if ((bind(s, (struct sockaddr *)&sock_name,
         sizeof(sock_name))) == -1) {
         perror ("MSGserver: unable to bind to socket");
         finished = 1;
         }
      }
                                         /* UNIX domain */
   else {
      domain_type = 1;
      if ((device = getenv(getenv("TTDEV"))) == NULL) {
                                        /* undefined                  */
         fprintf (stderr, "MSGserver: NO SUCH DEVICE = TTDEV\n");
         sleep (OOPSDELAY);
         exit(errno);
         }
      server_un.sun_family = AF_UNIX;
      if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
         perror ("MakeLink: socket (UNIX)");
         sleep (OOPSDELAY);
         exit(errno);
         }
                                         /* Otherwise, open socket    */
      unlink (device);                   /* first unlink if it exists */
      fprintf (stderr,
         "MSGserver: Starting AIPS task logging, Unix (local) domain\n\n");
      strcpy (server_un.sun_path, device);
      strcpy (msgunixdev, device);
      if (bind (s, (struct sockaddr *)&server_un,
         strlen(server_un.sun_path) + 2) < 0) {
         perror ("MakeLink: bind error (UNIX)");
         finished = 1;
         }
      }
   fprintf (stderr, "hostna> task #: Message\n");
   fprintf (stderr, "----------------------------------------\n");
                                        /* Since we are the server,   */
                                        /* listen for a connection.   */
   if (! finished) {
      if (listen(s, 5) == -1) {
         perror("MSGserver: listening on socket failed");
         finished = 1;
         }
      }
                                        /* Open the window            */
   if (! finished) {
      if ((fd = open(ttyname(1), O_WRONLY)) == -1) {
         perror ("MSGserver: Unable to open tty in write-only mode");
         finished = 1;
         }
      }
                                        /* empty previous contents!!  */
   for (i=0; i<sizeof(buf); i++)
      buf[i] = '\0';
                                        /* Loop until error - forever */
   while (! finished) {
                                        /* Get incoming connection.   */
      if (domain_type == 0) {
         in_addr_len = sizeof(in_addr);
         if ((snew = accept(s, (struct sockaddr *) &in_addr,
            &in_addr_len)) == -1) {
            perror("MSGserver: could not accept on the socket");
            finished = 1;
            }
                                        /* get hostname of source     */
         if (! finished) {
#if __STDC__
            remhost = gethostbyaddr((const char *) &(in_addr.sin_addr),
               sizeof(in_addr.sin_addr), AF_INET);
#else
            remhost = gethostbyaddr((char *) &(in_addr.sin_addr),
               sizeof(in_addr.sin_addr), AF_INET);
#endif
            if (remhost == NULL) {
                                        /* increment the error count */
               if (nbadh == 0) {
                  perror
                     ("MSGserver: cannot determine originating host");
                  strncpy (rhost, "??????", 6);
                  rhost[7] = '\0';
                  nbadh = 1;
                  }
               else {
                  strncpy (rhost, "??????", 6);
                  }
               }
            else {
               strncpy (rhost, remhost->h_name, sizeof(rhost));
               rhost[sizeof(rhost)-1] = '\0';
               }
            }
         }
      else {               /* UNIX_DOMAIN                */
         len = sizeof(from_un);
         if ((snew = accept (s, (struct sockaddr *) &from_un, &len))
            < 0) {
            perror ("MSGserver: could not accept on the socket");
            finished = 1;
            }
         if ((lochost = getenv("HOST")) == NULL) {
            strncpy (rhost, "??????", 6);
            }
         else {
            strncpy (rhost, lochost, sizeof(rhost));
            rhost[sizeof(rhost)-1] = '\0';
            }
         }
                                        /* read a line from socket    */

      if (! finished) {
         while ((ic = read (snew, buf, BUFLEN)) > 0) {
            if ((buf[0] == '\003') && (buf[1] == buf[0])) {
	       strcpy (buf, "MSGserver: told to close down");
               ic = 0;
               finished = 1;
               }
                                        /* output host> message       */
            sprintf(outbuf, "%-6s> %s\n", rhost, buf);
                                        /* empty previous contents!!  */
            for (i = 0; i <= ic; i++)
               buf[i] = '\0';
                                        /* to terminal now            */
            ic = strlen(outbuf) + 1;
            if (write(fd, outbuf, ic) != ic) {
               perror ("MSGserver: Bad write to the terminal");
               finished = 1;
               }
            if (write (snew, "OK", 2) != 2) {
               perror ("MSGserver: Failed to send 'OK' to client");
               finished = 1;
               }
            }
         }
                                        /* Close the connection.      */
      if (close(snew) == -1) {
         perror ("MSGserver: could not close connection");
         finished = 1;
         }
      }                                 /* end while (! finished)     */

   sleep (OOPSDELAY);
   close (fd);
   shutdown(s,0) ;
   close(s);
   if (domain_type == 1) unlink (msgunixdev);
   exit (errno);
}

#if __STDC__
   void zmsabo_(int isig)
#else
   void zmsabo_(isig)
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
      fprintf (stderr, "ZMSABO: signal %d received\n", i);
      shutdown(s,0) ;
      close(s);
      if (domain_type == 1) unlink (msgunixdev);
      sleep (OOPSDELAY);
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

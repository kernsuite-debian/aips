#define Z_vtpo3__
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
struct sockaddr_un fromb, serverb;
struct sockaddr_in fromb_in, serverb_in;
struct servent *spb_in;
int Z_sndbuf, Z_rcvbuf, Z_sndini, Z_rcvini;

                                        /* Use 1 for debugging */
#define DEBUGGING 0

#if __STDC__
   void zvtpo3_(int *hlen, char hsock[8], char *thosts, int *thlen,
             int *nth, int *fcb, int *ierr)
#else
   void zvtpo3_(hlen, hsock, thosts, thlen, nth, fcb, ierr)
   int *thlen, *nth, *fcb, *ierr, *hlen;
   char hsock[8], *thosts;
#endif
/*--------------------------------------------------------------------*/
/*! open connection in server (real-TP) to client (virtual-TP)        */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2000                                          */
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
/*  ZVTPO3 opens the connection in TPMON to the remote machine which  */
/*  is running the AIPS Virtual tape code.                            */
/*  Outputs:                                                          */
/*     fcb   I(*)   File descriptor                                   */
/*     ierr  I      Error: 0 => okay                                  */
/*                     1 => failure                                   */
/*                     2 => invalid device name                       */
/*                     3 => permission refused (invalid remote host)  */
/*   BSD UNIX version                                                 */
/*   Create a socket to receive commands/requests from remote         */
/*   computers for TP service.  Authenticate each connect.            */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
              Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   char device[48];
                                        /* remote client entry        */
   struct hostent *remhost;
   char rhost[41], thishost[41], temp[41], ch, *ipaddr, *ptr;
   int i, j, k, lr, lt, len, sfd, cfd, optlen, auth;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;

   for (i=0; i < 48 && hsock[i] != ':' && hsock[i] != '\0'; i++)
     device[i] = hsock[i];
   device[i] = '\0';
                                        /* open socket, INET domain   */
   if (strncmp(device,"aipsmt",6)==0) {
                                        /* translate name             */
      if ((spb_in = getservbyname (device,"tcp")) == NULL) {
         fprintf(stderr,"ZVTPO3: tcp/%s is not a service\n", device);
         fprintf(stderr,"ZVTPO3: check /etc/services or NIS map!\n");
         *ierr = 2;
         goto exit;
         }
      serverb_in.sin_port = spb_in->s_port;
      if ((sfd = (int)socket(AF_INET, SOCK_STREAM, 0)) < 0) {
         perror("ZVTPO3 socket (INET)");
         *ierr = 1;
         goto exit;
         }
      *(fcb+Z_fcbfd) = sfd;
      if (bind(sfd, (struct sockaddr *) &serverb_in,
               sizeof(serverb_in)) < 0) {
         perror("ZVTPO3 bind (INET)");
         *ierr = 1;
         *(fcb + Z_fcberr) = errno;
         goto cleanup;
         }
      listen(sfd, 5);
    next_accept:
      len = sizeof(fromb_in);
      if ((cfd = accept(sfd,
                	(struct sockaddr *) &fromb_in, &len)) < 0) {
	 perror("ZVTPO3 accept (INET)");
	 *ierr = 1;
	 goto cleanup;
      } else {
	 *(fcb+Z_fcbsp1) = cfd;
         }
#if __STDC__
      remhost = gethostbyaddr((const char *) &(fromb_in.sin_addr),
			      sizeof(fromb_in.sin_addr), AF_INET);
#else
      remhost = gethostbyaddr((char *) &(fromb_in.sin_addr),
			      sizeof(fromb_in.sin_addr), AF_INET);
#endif
      if (remhost == NULL) {
	 perror ("ZVTPO3 cannot get remote host info");
	 shutdown(cfd, 0);
	 close(cfd);
	 goto next_accept;
                                        /* was ierr 1, goto cleanup */
         }
      strncpy (rhost, remhost->h_name, sizeof(rhost));
      rhost[sizeof(rhost)-1] = '\0';
      lr = strlen(rhost);
                                        /* keep copy of official name */
                                        /* same size but be paranoid! */
      strncpy(temp, rhost, sizeof(temp));
                                        /* get IP address just in */
                                        /* case we need it */
      ipaddr = inet_ntoa(fromb_in.sin_addr);
      for (i=0; i<lr; i++)
	rhost[i] = toupper(rhost[i]);
#if DEBUGGING
      fprintf (stderr, "TPMON Received request from %s (%s)\n",
               rhost, ipaddr);
#endif
                                        /* loop through official name */
                                        /* and any possible aliases   */
      auth = 0;
      while ((rhost[0] != '\0') && (auth == 0)) {
                                        /* Need to authenticate this */
                                        /* host before proceeding.   */
	 lr = strlen(rhost);
                                        /* check every entry in */
                                        /* "allowed" list */
	 for (i=0; i<*nth; i++) {
	    j = i * *thlen * 4;
	    ch = thosts[j];
	    if (ch == ' ') {
	       break;
	    } else {
                                        /* get name for convenience */
	       memcpy(thishost, thosts+j, ((*thlen) * 4)-1);
	       thishost[39] = ' ';
                                        /* null-pad it */
	       for (k=39; k>0 && thishost[k] != ' '; k--) {
		  thishost[k] = '\0';
                  }
               lt = strlen(thishost);
#if DEBUGGING
               fprintf(stderr, "TPMON comparing with %s\n", thishost);
#endif
                                        /* use switch in case we want */
                                        /* to add additional wild */
                                        /* cards later... */
               switch (ch) {
                                        /* Crude wildcarding.  Ugly! */
                                        /* First character is a star */
                case '*':
                  if (memcmp(thishost+1, rhost+(lr+1-lt), lt-1) == 0) {
                     auth = 1;
#if DEBUGGING
                     fprintf(stderr, "TPMON, matched alpha wildcard\n");
#endif
                     }
                  break;
                                        /* IP address, possibly */
                case '1': case '2': case '3': case '4': case '5':
                case '6': case '7': case '8': case '9': case '0':
                                        /* catch trailing wildcard */
                  ch = thishost[lt-1];
                  switch (ch) {
                   case '*':
                     if (memcmp(ipaddr, thishost, lt-1) == 0) {
                        auth = 1;
#if DEBUGGING
                        fprintf(stderr, "TPMON, matched IP wildcard\n");
#endif
                        }
                     break;
                   default:
                     if (memcmp (ipaddr, thishost, lr) == 0) {
                        auth = 1;
#if DEBUGGING
                        fprintf(stderr, "TPMON, matched IP exactly\n");
#endif
                        }
                     break;
                     }
                  break;
                default:
                  if (memcmp(rhost, thishost, lr) == 0) {
                     auth = 1;
#if DEBUGGING
                     fprintf(stderr, "TPMON, matched host exactly\n");
#endif
                     }
                  }                     /* end switch */
               }                        /* end check on ch blank */
            if (auth == 1)
              break;
         }                              /* end loop over host list */
                                        /* check for aliases */
         ptr = *(remhost->h_aliases);
         if (ptr != NULL) {
            strncpy(rhost, ptr, sizeof(rhost));
            remhost->h_aliases++;
            lr = strlen(rhost);
            for (k=0; k <lr; k++)
              rhost[k] = toupper(rhost[k]);
#if DEBUGGING
            fprintf(stderr, "TPMON, next alias is %s\n", rhost);
#endif
         } else {
                                        /* no more aliases, end loop */
            rhost[0] = '\0';
            }
         }
      if (auth == 0) {
         fprintf (stderr,
                  "ZVTPO3: permission REFUSED for host %s (%s)\n",
                  temp, ipaddr);
                                        /* shutdown connection and */
                                        /* prepare to accept next */
         shutdown (cfd, 0);
         close (cfd);
         goto next_accept;
      } else {
#if DEBUGGING
         fprintf(stderr, "TPMON, letting this one in\n");
#endif
         optlen = sizeof (Z_sndbuf);
         if (getsockopt (cfd, SOL_SOCKET, SO_SNDBUF, (char *) &Z_sndbuf,
                         &optlen) < 0) {
            perror("ZVTPO3 SO_SNDBUF get");
            Z_sndbuf = 4096;
            }
/*       else
           fprintf (stderr,
                    "Default SO_SNDBUF is %d bytes\n", Z_sndbuf); */
         optlen = sizeof (Z_rcvbuf);
         if (getsockopt (cfd, SOL_SOCKET, SO_RCVBUF, (char *) &Z_rcvbuf,
                         &optlen) < 0) {
            perror("ZVTPO3 SO_RCVBUF get");
            Z_rcvbuf = 4096;
            }
/*       else
           fprintf (stderr,
                    "Default SO_RCVBUF is %d bytes\n", Z_rcvbuf); */
         Z_sndini = Z_sndbuf;
         Z_rcvini = Z_rcvbuf;
         }
      }
                                        /* UNIX domain not in VTP     */
   else {
      fprintf(stderr,"ZVTPO3: UNIX domain not supported\n");
      *ierr = 2;
      }
                                        /* system error # in FTAB     */
 exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   goto final;

 cleanup:
   if (shutdown(sfd, 0) != 0) {
      *ierr += 2;
      }
   if (close(sfd) != 0) {
      *ierr += 1;
      }

 final:
   return;
}

#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_ignal__
#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <AIPSPROTO.H>
#if __STDC__
   void (*zignal_ (int signo, void (*func)(int)))(int)
#else
   void (*zignal_ (signo, func))()
   int signo;
   void (*func)();
#endif
/*--------------------------------------------------------------------*/
/*! establishes abort handling through call to sigaction              */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
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
/* reliable version of signal() using POSIX sigaction()               */
/* from W. Richard Stevens "Advanced Programming in the UNIX Envir."  */
/*  Inputs:                                                           */
/*     signo   I   Signal number                                      */
/*     func    F   Function to call: no return, arg int               */
/*  Output:                                                           */
/*     zignal_ F   Previous Function to call: no return, arg int      */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
   struct sigaction  act, oact;

   act.sa_handler = func;
   sigemptyset (&act.sa_mask);
   act.sa_flags = 0;
   if (signo == SIGALRM) {
#ifdef SA_INTERRUPT
      act.sa_flags |= SA_INTERRUPT;
#endif
      }
   else {
#ifdef SA_RESTART
      act.sa_flags |= SA_RESTART;
#endif
      }

   if (sigaction (signo, &act, &oact) < 0)
      return (SIG_ERR);
   else
      return (oact.sa_handler);
}

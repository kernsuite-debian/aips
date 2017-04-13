#define Z_prmpt__
#include <stdio.h>
#include <ctype.h>

#if __STDC__
   void zprmpt_(char ipc[1], char buff[80], int *ierr)
#else
   zprmpt_(ipc, buff, ierr)
   char ipc[1], buff[80];
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! prompt user and read 80-characters from CRT screen                */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*--------------------------------------------------------------------*/
/* ZPRMPT prompts user on CRT screen and reads a line (80 characters  */
/* maximum).                                                          */
/* Input:                                                             */
/*    IPC    C*1    prompt character                                  */
/* Output:                                                            */
/*    BUFF   C*80   line of user input.                               */
/*    IERR   I      Error return code: 0 => ok                        */
/*                     1 => read/write error                          */
/* Generic UNIX version.                                              */
/*--------------------------------------------------------------------*/
{
   char pc, temp[81];
   register int i;
/*--------------------------------------------------------------------*/
   *ierr = 0;

   buff[0] = '\0';                      /* Initialize buffers.        */
   temp[0] = '\0';

   pc = ipc[0];                         /* Get prompt character.      */

   putchar(pc);                         /* Issue prompt.              */

                                        /* Get what user types.       */
   if (fgets(temp, 81, stdin) == NULL)
                                        /* This is necessary in case  */
                                        /* a user puts the control    */
                                        /* process (e.g., AIPS) in    */
                                        /* the background, continues  */
                                        /* it and logs out.           */
                                        /* Otherwise, the control     */
                                        /* process starts taking its  */
                                        /* terminal input from        */
                                        /* from /dev/null and goes    */
                                        /* into an infinite loop.     */
      sprintf(buff,"EXIT");
                                        /* Strip off newline and form */
                                        /* blank filled buffer        */
   else {
      for (i = 0; i < 80 && temp[i] != '\012'; i++)
         buff[i] = temp[i];
      for (i=i; i < 80; i++)
         buff[i] = ' ';
      }

   return;
}

      SUBROUTINE QINIT (I1, I2, APNUM)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Initialize "AP".
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2008-2009, 2012-2013, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Initialize the AP.  For the pseudo-AP the size of the arrays in
C   COMMON /APCORE/ are defined and the AP roller common is initialized
C   Dynamic memory will be allocated here.
C   Inputs:
C      I1      I   Desired size of AP in kilowords
C                  allocated = max (min (I1, KAPWRD), PSAPMN)
C      I2      I   Dummy
C   Outputs:
C      APNUM   I   AP number (Neg. to indicate virtual AP, ie. not
C                   to be rolled.  0 => ERROR NO MEMORY ASSIGNED
C   Output to common /BPROLC/
C      TRUEAP  L   True if a real AP (to be rolled) (FALSE)
C      XTLAST  D   Real time AP assigned (min).
C      DELTIM  D   Time interval between rolls (min).
C      DELAY   R   Time to delay task (seconds).
C   Common /SPF/  (DAPM.INC)
C      PSAPNW  I   Number words in PSAP (0 => none allocated)
C      PSAPOF  LI  Pointer into dynamic memory (0 => none allocated)
C-----------------------------------------------------------------------
      INTEGER   I1, I2, APNUM
C
      INTEGER   I, NWORDS, IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPC.INC'
      INCLUDE 'INCS:DBPR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Mark as non-rollable AP
      APNUM = -1
C                                       Fill values into /BPROLC/
      TRUEAP = .FALSE.
C                                       Delay time = 10 sec.
      DELAY = 10.0
C                                       Time between rolls = 5 min.
      DELTIM = 5.0D0
C                                       Set time.
      XTLAST = 0.0
C                                       Allowed range
      NWORDS = MAX (MIN (I1, KAPWRD+512), PSAPMN)
C                                       static memory
      IF (PKPWRD.GT.1000) THEN
         PSAPNW = (PKPWRD-1) / 1024
         PSAPOF = 1
         IF (NWORDS.GT.PSAPNW) THEN
            WRITE (MSGTXT,1000) NWORDS, PSAPNW
            CALL MSGWRT (7)
            END IF
C                                       need to allocate memory
      ELSE IF (NWORDS.GT.PSAPNW) THEN
C                                       deallocate old memory
         IF ((PSAPNW.GT.0) .AND. (PSAPOF.NE.0) .AND. (PSAPOF.NE.1)) THEN
            WRITE (MSGTXT,1010) 'FREE', PSAPNW, PSAPOF
            CALL MSGWRT (2)
            I = PSAPNW * 2048
            PSAPOF = 2 * PSAPOF - 1
            PSAPOF = PSAPOF - 1
            CALL ZMEMRY ('FREE', TSKNAM, I, APCORE, PSAPOF, IERR)
            IF (IERR.NE.0) THEN
               I = I / 1024
               WRITE (MSGTXT,1005) IERR, 'FREE', I
               CALL MSGWRT (7)
               END IF
            END IF
C                                       allocate new memory
         PSAPNW = MAX (1, NWORDS)
         I = PSAPNW * 2
         CALL ZMEMRY ('GET ', TSKNAM, I, APCORE, PSAPOF, IERR)
         IF (IERR.NE.0) THEN
            I = I / 2
            WRITE (MSGTXT,1005) IERR, 'GET', I
            CALL MSGWRT (7)
            PSAPNW = 0
            APNUM = 0
            PSAPOF = 0
         ELSE
            PSAPOF = PSAPOF + 1
C                                       double precision
            PSAPOF = (PSAPOF + 1) / 2
            WRITE (MSGTXT,1010) 'GET ', PSAPNW, PSAPOF
            CALL MSGWRT (2)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('QINIT:',I10,' KWORDS REQUESTED',I10,' KWORDS AVAILABLE')
 1005 FORMAT ('QINIT: ERROR',I3,' FROM ZMEMRY DOING ',A,' KWORDS',I12)
 1010 FORMAT ('QINIT: did a ',A,' of',I10,' Kwords, OFF',I17)
      END

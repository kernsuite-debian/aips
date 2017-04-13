      SUBROUTINE ZDEAX2 (FCB, IOP, OPER, BUFF, NBYTE, PA, PB, WAIT,
     *   IERR)
C-----------------------------------------------------------------------
C! do actual read/write from/to DeAnza device
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Queue an I/O operation to the DeAnza TV device, and wait for
C   completion if requested.
C   Inputs:
C       FCB     I(*)    File control block for IO
C       IOP     I       Operation code number from list
C       OPER    C*4     Operation code (duplicates IOP info)
C       BUFF    I(?)    User i/o buffer
C       NBYTE   I       Number of bytes to be transferred
C       PA      I       1st extra parameter
C       PB      I       2nd extra parameter
C       WAIT    L       If .TRUE. wait for completion before return
C   Outputs:
C       IERR    I
C    Calls to ZDEAXF map one to one to calls to IP8 routines in the
C    DeAnza IP8500 level 0 software package.  I replaced them here for
C    the following reasons:
C    1)  I don't like EXTERNALs scattered all over the code, so
C        now they are all concentrated here in 1 subroutine.
C    2)  If you want to do any asynchronous calls, the event
C        flag numbers, IOSBs and other such structures are
C        now contained in the standard AIPS FTAB structure, instead
C        of being buried in IP8500 code.  Thus calls to
C        ZWAIT (TVLUN, IERR) etc. will do the right thing.  I do
C        not use this in the current implimentation.
C   This effort may have been misguided, but now its done.  If you
C   want to go back to IP8 calls, it won't be much work.
C                               W. Jaffe     Nov-1983
C   Generic version  - stubbed with error return (commented code from
C   VMS version)
C-----------------------------------------------------------------------
      CHARACTER OPER*4
      INTEGER   FCB(*), IOP, BUFF(*), NBYTE, PA, PB, IERR
      LOGICAL   WAIT
C
C      EXTERNAL  DAT, DET, LR, RR, WI, RI, LCR, RCR, BMC, LMC,
C     *          LPR, RPR, LVR, RVR,
C     *          LWA, RWA, LPA, RPA, WR, BPA
C     INTEGER   QUERR, SYS$QIO, SYS$QIOW, FUNC
C     INTEGER   QUERR, FUNC, LOCPA, LOCPB, JERR
C     CHARACTER CERR*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Needs development under UNIX
      IERR = 3
      WRITE (MSGTXT,9999)
      CALL MSGWRT (8)
      IF (NLUSER.GT.0) GO TO 999
C                               Find OPCODE
C      IF (IOP.EQ.1) THEN
C         FUNC = %LOC(DAT)
C      ELSE IF (IOP.EQ.2) THEN
C         FUNC = %LOC(DET)
C      ELSE IF (IOP.EQ.3) THEN
C         FUNC = %LOC(LR)
C      ELSE IF (IOP.EQ.4) THEN
C         FUNC = %LOC(RR)
C      ELSE IF (IOP.EQ.5 ) THEN
C         FUNC = %LOC(WI)
C      ELSE IF (IOP.EQ.6) THEN
C         FUNC = %LOC(RI)
C      ELSE IF (IOP.EQ.7) THEN
C         FUNC = %LOC(LCR)
C      ELSE IF (IOP.EQ.8) THEN
C         FUNC = %LOC(RCR)
C      ELSE IF (IOP.EQ.9) THEN
C         FUNC = %LOC(BMC)
C      ELSE IF (IOP.EQ.10) THEN
C         FUNC = %LOC(LMC)
C      ELSE IF (IOP.EQ.11) THEN
C         FUNC = %LOC(LPR)
C      ELSE IF (IOP.EQ.12) THEN
C         FUNC = %LOC(RPR)
C      ELSE IF (IOP.EQ.13) THEN
C         FUNC = %LOC(LVR)
C      ELSE IF (IOP.EQ.14) THEN
C         FUNC = %LOC(RVR)
C      ELSE IF (IOP.EQ.15) THEN
C         FUNC = %LOC(LWA)
C      ELSE IF (IOP.EQ.16) THEN
C         FUNC = %LOC(RWA)
C      ELSE IF (IOP.EQ.17) THEN
C         FUNC = %LOC(LPA)
C      ELSE IF (IOP.EQ.18) THEN
C         FUNC = %LOC(RPA)
C      ELSE IF (IOP.EQ.19) THEN
C         FUNC = %LOC(WR)
C      ELSE IF (IOP.EQ.20) THEN
C         FUNC = %LOC(BPA)
C      ELSE
C         IERR = 2
C         GO TO 999
C         END IF
C      LOCPA = PA
C      LOCPB = PB
C                                      Set up IO call
C      IF (WAIT) THEN
C         QUERR = SYS$QIOW (%VAL(FCB(2)), %VAL(FCB(1)), %VAL(FUNC),
C     *      FCB(3),,,BUFF, %VAL(NBYTE), %VAL(LOCPA),%VAL(LOCPB),,)
C         IF (QUERR.NE.1) THEN
C            IERR = 3
C            FCB(3) = QUERR
C         ELSE
C            JERR = FCB(3)
C            IF (JERR.NE.1) THEN
C               IERR = 3
C               CALL ZHEX (JERR, 8, CERR)
C               ENCODE (80,1000,MSGTXT) CERR
C               CALL MSGWRT (8)
C               END IF
C            END IF
C      ELSE
C         QUERR = SYS$QIO (%VAL(FCB(2)), %VAL(FCB(1)), %VAL(FUNC),
C     *      FCB(3),,, BUFF, %VAL(NBYTE), %VAL(LOCPA),%VAL(LOCPB),,)
C         IF (QUERR.NE.1) THEN
C            IERR = 3
C            FCB(3) = QUERR
C            END IF
C         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDEAXF ERROR ', A)
 9999 FORMAT ('ZDEAXF: REQUIRES LOCAL DEVELOPMENT')
      END

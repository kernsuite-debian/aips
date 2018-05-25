      SUBROUTINE ZDEAX2 (FCB, IOP, OP, BUFF, NBYTE, PA, PB, WAIT, IERR)
C-----------------------------------------------------------------------
C! do actual read/write from/to DeAnza device
C# Z2 TV-IO
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
C       IOP     I       Operation code number from list
C       OP      C*4     Operation code (duplicates IOP info)
C       NBYTE   I       Number of bytes to be transferred
C       PA      I       1st extra parameter
C       PB      I       2nd extra parameter
C       WAIT    L       If .TRUE. wait for completion before return
C   In/out:
C       FCB     I(*)    File control block for IO
C       BUFF    I(*)    User i/o buffer
C   Outputs:
C       IERR    I
C   VAX version
C   Calls to ZDEAXF map one to one to calls to IP8 routines in the
C   DeAnza IP8500 level 0 software package.  I replaced them here for
C   the following reasons:
C   1)  I don't like EXTERNALs scattered all over the code, so
C     now they are all concentrated here in 1 subroutine.
C   2)  If you want to do any asynchronous calls, the event
C     flag numbers, IOSBs and other such structures are
C     now contained in the standard AIPS FTAB structure, instead
C     of being buried in IP8500 code.  Thus calls to
C     ZWAIT (TVLUN, IERR) etc. will do the right thing.  I do
C     not use this in the current implimentation.
C   This effort may have been misguided, but now its done.  If you
C   want to go back to IP8 calls, it won't be much work.
C                               W. Jaffe     Nov-1983
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   FCB(*), IOP, BUFF(*), NBYTE, PA, PB, IERR
      LOGICAL   WAIT
C
      EXTERNAL  DAT, DET, LR, RR, WI, RI, LCR, RCR, BMC, LMC, LPR, RPR,
     *   LVR, RVR, LWA, RWA, LPA, RPA, WR, BPA
      INTEGER   QUERR, SYS$QIO, SYS$QIOW, FUNC, LOCPA, LOCPB, JERR
      INTEGER*2 IOSB(4)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                               Find OPCODE
      IF (IOP.EQ.1) THEN
         FUNC = %LOC(DAT)
      ELSE IF (IOP.EQ.2) THEN
         FUNC = %LOC(DET)
      ELSE IF (IOP.EQ.3) THEN
         FUNC = %LOC(LR)
      ELSE IF (IOP.EQ.4) THEN
         FUNC = %LOC(RR)
      ELSE IF (IOP.EQ.5) THEN
         FUNC = %LOC(WI)
      ELSE IF (IOP.EQ.6) THEN
         FUNC = %LOC(RI)
      ELSE IF (IOP.EQ.7) THEN
         FUNC = %LOC(LCR)
      ELSE IF (IOP.EQ.8) THEN
         FUNC = %LOC(RCR)
      ELSE IF (IOP.EQ.9) THEN
         FUNC = %LOC(BMC)
      ELSE IF (IOP.EQ.10) THEN
         FUNC = %LOC(LMC)
      ELSE IF (IOP.EQ.11) THEN
         FUNC = %LOC(LPR)
      ELSE IF (IOP.EQ.12) THEN
         FUNC = %LOC(RPR)
      ELSE IF (IOP.EQ.13) THEN
         FUNC = %LOC(LVR)
      ELSE IF (IOP.EQ.14) THEN
         FUNC = %LOC(RVR)
      ELSE IF (IOP.EQ.15) THEN
         FUNC = %LOC(LWA)
      ELSE IF (IOP.EQ.16) THEN
         FUNC = %LOC(RWA)
      ELSE IF (IOP.EQ.17) THEN
         FUNC = %LOC(LPA)
      ELSE IF (IOP.EQ.18) THEN
         FUNC = %LOC(RPA)
      ELSE IF (IOP.EQ.19) THEN
         FUNC = %LOC(WR)
      ELSE IF (IOP.EQ.20) THEN
         FUNC = %LOC(BPA)
      ELSE
         IERR = 2
         GO TO 999
      END IF
C                                       Default parameters
      LOCPA = PA
      LOCPB = PB
C                                       Set up IO call
      FCB(1+FCBERR) = 0
      FCB(1+FCBXFR) = 0
      IF (WAIT) THEN
         QUERR = SYS$QIOW (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FUNC), IOSB, , , BUFF, %VAL(NBYTE), %VAL(LOCPA),
     *      %VAL(LOCPB), , )
         FCB(1+FCBXFR) = IOSB(2)
         IF (QUERR.NE.1) THEN
            IERR = 3
            FCB(1+FCBERR) = QUERR
         ELSE
            JERR = IOSB(1)
            IF (JERR.NE.1) THEN
               FCB(1+FCBERR) = JERR
               IERR = 3
               END IF
            END IF
      ELSE
         QUERR = SYS$QIO (%VAL(FCB(1+FCBSP1)), %VAL(FCB(1+FCBFD)),
     *      %VAL(FUNC), FCB(1+FCBSP2), , , BUFF, %VAL(NBYTE),
     *      %VAL(LOCPA), %VAL(LOCPB), , )
         IF (QUERR.NE.1) THEN
            IERR = 3
            FCB(1+FCBERR) = QUERR
            END IF
         END IF
C
 999  RETURN
      END

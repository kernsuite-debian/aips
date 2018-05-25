      SUBROUTINE ZDEAXF (OPER, BUFF, NBYTE, PA, PB, WAIT, IERR)
C-----------------------------------------------------------------------
C! do IO to DeAnza TV
C# Z TV-IO
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
C       OPER    C*4     Operation code
C       BUFF    I(?)    User i/o buffer
C       NBYTE   I       Number of bytes to be transferred
C       PA      I       1st extra parameter
C       PB      I       2nd extra parameter
C       WAIT    L       If .TRUE. wait for completion before return
C  Outputs:
C       IERR    I       Error code
C   Calls to ZDEAXF map one to one to calls to IP8 routines in the
C   DeAnza IP8500 level 0 software package.  I replaced them here for
C   the following reasons:
C   (1)  I don't like EXTERNALs scattered all over the code, so now they
C        are all concentrated here in 1 subroutine.
C   (2)  If you want to do any asynchronous calls, the event flag
C        numbers, IOSBs and other such structures are now contained in
C        the standard AIPS FTAB structure, instead of being buried in
C        IP8500 code.  Thus calls to ZWAIT (TVLUN, IERR) etc. will do
C        the right thing.  I do not use this in the current
C        implimentation.
C   This effort may have been misguided, but now its done.  If you
C   want to go back to IP8 calls, it won't be much work.
C                               W. Jaffe     Nov-1983
C   Generic version - calls ZDEAX2, ZIPACK to make 16-bit buffers
C-----------------------------------------------------------------------
      CHARACTER OPER*(*)
      INTEGER   BUFF(*), NBYTE, PA, PB, IERR
      LOGICAL   WAIT
C
      CHARACTER OPERS(20)*4
      INTEGER   NOPERS, FCBOFF, IOP, TVBUFF(2048), NWORD
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA NOPERS, OPERS /20, 'DAT ','DET ','LR  ','RR  ','WI  ',
     *   'RI  ','LCR ','RCR ','BMC ','LMC ','LPR ','RPR ','LVR ',
     *   'RVR ','LWA ','RWA ','LPA ','RPA ','WR  ','BPA '/
C-----------------------------------------------------------------------
C                                       check OPER
      DO 10 IOP = 1,NOPERS
         IF (OPER.EQ.OPERS(IOP)) GO TO 20
 10      CONTINUE
      WRITE (MSGTXT,1010) OPER
      IERR = 2
      GO TO 995
C                                       Not open
 20   IF (FTAB(TVIND).EQ.TVLUN) GO TO 30
         WRITE (MSGTXT,1020)
         IERR = 1
         GO TO 995
C                                       locate UFT
 30   FCBOFF = TVIND + NMOFF
C                                       pack to 16 bits
      NWORD = (NBYTE + 1) / 2
      CALL ZIPACK ('LTOS', NWORD, BUFF, 1, TVBUFF)
C                                       Set up IO call
      CALL ZDEAX2 (FTAB(FCBOFF), IOP, OPER, TVBUFF, NBYTE, PA, PB,
     *   WAIT, IERR)
C                                       unpack to 32 bits
      IF (IERR.EQ.0) THEN
         CALL ZIPACK ('STOL', NWORD, BUFF, 1, TVBUFF)
C                                       or report error
      ELSE
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (7)
         CALL ZERROR ('ZDEAX2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *      F)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZDEAXF: OPER ''',A4,''' NOT RECOGNIZED')
 1020 FORMAT ('ZDEAXF: TV NOT OPEN IN FTAB')
 1030 FORMAT ('ZDEAXF: ERROR',I5,' RETURNED BY ZDEAX2')
      END

      SUBROUTINE ZSTAIP
C-----------------------------------------------------------------------
C! does any system cleanup needed at the end of interactive AIPS session
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   ZSTAIP performs any operations needed to normalize the local
C   operating system at the conclusion of an interactive AIPS session.
C   Outputs:
C      SCRTCH   I(256)   Scratch buffer
C   Generic version  - calls ZSTAI2 in case there is something to do
C   in C or Macro, else a null routine.
C
C   AIPS runs as a child process to $UNIX/AIPSTR between executions of
C   ZSTRTA (estabishes the AIPS environment) and ZSTOPA (cleans up).
C
C   Under VMS, AIPS runs as an overlay to normal job control and all
C   clean-up is done by AIPS.COM procedure and ZSTOPA.
C-----------------------------------------------------------------------
C
      INTEGER   IDRIVE, IDUM, JERR, I, J, ITRIM, RDRIVE, SYSERR
      CHARACTER CDUM*8, PNAME*48, XLATED*132, LMSG*80
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DBBF.INC'
C-----------------------------------------------------------------------
      IF ((TSKNAM.EQ.'AIPS') .OR. (TSKNAM.EQ.'GR2TEX') .OR.
     *   (TSKNAM.EQ.'GRITP') .OR. (TSKNAM.EQ.'TPMON')) THEN
C                                       Dismount mounted tapes
         DO 20 IDRIVE = 1,NTAPED
            IF (TPNAME(IDRIVE).NE.' ') THEN
C                                       Always do remote tapes
               IF (TPNAME(IDRIVE).NE.'LOCAL') THEN
C                                       Real tape
                  PNAME = 'AMT0x:'
                  CALL ZEHEX (IDRIVE, 1, PNAME(5:5))
                  CALL ZTRLOG (6, PNAME, 128, XLATED, J, JERR)
                  RDRIVE = 0
                  IF ((JERR.EQ.0) .AND. (XLATED(:9).NE.'UNMOUNTED'))
     *               THEN
                     CALL ZHEX10 (XLATED(7:7), RDRIVE, I)
                     IF ((RDRIVE.GT.0) .AND. (I.EQ.0)) THEN
                        I = ITRIM (TPNAME(IDRIVE))
                        WRITE (MSGTXT,1000) IDRIVE, RDRIVE,
     *                  TPNAME(IDRIVE)(:I)
                        CALL MSGWRT (2)
                        CALL ZMOUNT (.FALSE., IDRIVE, IDUM,
     *                     TPNAME(IDRIVE), RDRIVE, LMSG, SYSERR, JERR)
                        END IF
                     END IF
C                                       Local tapes may not want this
               ELSE
                  WRITE (MSGTXT,1010) IDRIVE
                  CALL MSGWRT (2)
                  CALL ZMOUNT (.FALSE., IDRIVE, IDUM, CDUM, IDUM,
     *               LMSG, SYSERR, JERR)
                  END IF
               END IF
20         CONTINUE
C                                       Call clean up service.
         CALL ZSTAI2 (NPOPS, NLUSER, BBUFR1)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZSTAIP: Dismounting remote tape',I2,' as drive',I2,
     *   ' on ',A)
 1010 FORMAT ('ZSTAIP: Dismounting local real tape drive',I2)
      END

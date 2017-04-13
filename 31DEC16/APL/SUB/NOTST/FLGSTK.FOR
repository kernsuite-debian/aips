      SUBROUTINE FLGSTK (STOKES, OPCODE, PFLAGS, LFSET, IRET)
C-----------------------------------------------------------------------
C! Set Stokes flag for uv flagging.
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008, 2014-2015
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
C   Subroutine to compute flag for UVFLG visibility flagging based on
C   STOKES and OPCODE.  Uses current values in /MAPHDR/ CATBLK
C   and the /UVHDR/ common (filled by UVPGET).
C   Input:
C      STOKES    C*4  Stokes' type,
C                     Recognized are 'I', 'Q', 'U', 'V', 'IQU',
C                     'IQUV', 'IV', 'RR', 'LL', 'LR', 'RL',
C                     'XX','YY','XY','YX','CROS'
C                     'HALF'(=RR,LL) and 'FULL' (=RR,LL,RL,LR)
C                     '    '=>all stokes' types.
C      OPCODE    C*4  'FLAG', 'UFLG' 'REAS' (=Unflag by REASON)
C   Output:
C      PFLAGS(4) L    Polarization flags, same order as in data.
C                     If data true Stokes' or correlator (baseline)
C                     flagging is requested all 4 are filled in;
C                     for IF (RR and/or LL) flagging only first 2
C                     are meaningful.
C      LFSET     L    .TRUE. if flag, .FALSE. if unflag.
C      IRET      I    Return code, 0=OK, else data incompatible
C                     with request.
C-----------------------------------------------------------------------
      CHARACTER STOKES*4, OPCODE*4
      INTEGER   IRET
      LOGICAL   PFLAGS(4), LFSET
C
      INTEGER   NSTOK
      PARAMETER (NSTOK = 23)
      CHARACTER CHSTOK(NSTOK)*4, CHMASK(16)*4, CHOPCO(5)*4
      INTEGER   NOPCO, ISTOK, IOPCO, I
      LOGICAL   T, F
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA NOPCO /5/
      DATA CHSTOK /'I', 'Q', 'U', 'V', 'IQU', 'IQUV', 'IV',
     *   'RR', 'LL', 'RL', 'LR', 'RRLL', 'RLLR',
     *   'XX', 'YY', 'XY', 'YX', 'XXYY', 'XYYX',
     *   'HALF','FULL','CROS',' '/
      DATA CHMASK /'1000','0100','0010','0001','1100','1010','1001',
     *   '0110','0101','0011','1110','1101','1011','0111','1111','0000'/
      DATA CHOPCO /'FLAG','UFLG','REAS','WILD','    '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IRET = 0
C                                       See if a mask
      ISTOK = 0
      DO 10 I = 1,16
         IF (STOKES.EQ.CHMASK(I)) ISTOK = I
 10      CONTINUE
C                                       parse the mask
      IF (ISTOK.GT.0) THEN
         DO 15 I = 1,4
            PFLAGS(I) = STOKES(I:I).EQ.'1'
 15         CONTINUE
         LFSET = (OPCODE.EQ.CHOPCO(1)) .OR. (OPCODE.EQ.CHOPCO(5))
         GO TO 999
         END IF
C                                       Find Stokes number
      DO 50 I = 1,NSTOK
         IF (STOKES.EQ.CHSTOK(I)) THEN
            ISTOK = I
            GO TO 60
            END IF
 50      CONTINUE
C                                       Unknown Stokes'
      WRITE (MSGTXT,1050) STOKES
      IRET = 9
      GO TO 990
C                                       Find OPCODE number
 60   DO 70 I = 1,NOPCO
         IF (OPCODE.EQ.CHOPCO(I)) THEN
            IOPCO = I
            GO TO 80
            END IF
 70      CONTINUE
C                                       Unknown OPCODE
      WRITE (MSGTXT,1070) OPCODE
      IRET = 10
      GO TO 990
C                                       Decide if flag or unflag.
 80   LFSET = (IOPCO.EQ.1) .OR. (IOPCO.EQ.5)
C                                       Decide if true stokes or
C                                       correlator (RR, LL etc.)
      PFLAGS(1) = F
      PFLAGS(2) = F
      PFLAGS(3) = F
      PFLAGS(4) = F
      IF (ICOR0.GE.0) THEN
C                                       True stokes - check
         IF ((ISTOK.GE.8) .AND. (ISTOK.NE.NSTOK)) GO TO 950
C                                       Same for antenna or baseline
C                                       based.
         IF ((ISTOK.EQ.1) .OR. (ISTOK.EQ.5) .OR. (ISTOK.EQ.6) .OR.
     *      (ISTOK.EQ.7) .OR. (ISTOK.EQ.NSTOK)) PFLAGS(1) = T
         IF ((ISTOK.EQ.2) .OR. (ISTOK.EQ.5) .OR. (ISTOK.EQ.6) .OR.
     *      (ISTOK.EQ.NSTOK)) PFLAGS(2) = T
         IF ((ISTOK.EQ.3) .OR. (ISTOK.EQ.5) .OR. (ISTOK.EQ.6) .OR.
     *      (ISTOK.EQ.NSTOK)) PFLAGS(3) = T
         IF ((ISTOK.EQ.4) .OR. (ISTOK.EQ.6) .OR. (ISTOK.EQ.7) .OR.
     *      (ISTOK.EQ.NSTOK)) PFLAGS(4) = T
C                                       Correlator based data (RR...)
      ELSE IF (ICOR0.GT.-5) THEN
         IF ((ISTOK.EQ.1) .OR. ((ISTOK.GE.4) .AND. (ISTOK.LE.8)) .OR.
     *      (ISTOK.EQ.12) .OR. (ISTOK.EQ.NSTOK) .OR.
     *      ((ISTOK.GE.19) .AND. (ISTOK.LE.21))) PFLAGS(1) = T
         IF ((ISTOK.EQ.1) .OR. ((ISTOK.GE.4) .AND. (ISTOK.LE.7)) .OR.
     *      (ISTOK.EQ.9) .OR. (ISTOK.EQ.12) .OR. (ISTOK.EQ.NSTOK) .OR.
     *      ((ISTOK.GE.19) .AND. (ISTOK.LE.21))) PFLAGS(2) = T
         IF ((ISTOK.EQ.2) .OR. (ISTOK.EQ.3) .OR. (ISTOK.EQ.5) .OR.
     *      (ISTOK.EQ.6) .OR. (ISTOK.EQ.10) .OR. (ISTOK.EQ.13) .OR.
     *      ((ISTOK.GE.21) .AND. (ISTOK.LE.NSTOK))) PFLAGS(3) = T
         IF ((ISTOK.EQ.2) .OR. (ISTOK.EQ.3) .OR. (ISTOK.EQ.5) .OR.
     *      (ISTOK.EQ.6) .OR. (ISTOK.EQ.11) .OR. (ISTOK.EQ.13) .OR.
     *      ((ISTOK.GE.21) .AND. (ISTOK.LE.NSTOK))) PFLAGS(4) = T
C                                       Correlator based data (XX...)
      ELSE
         IF ((ISTOK.EQ.1) .OR. ((ISTOK.GE.5) .AND. (ISTOK.LE.7)) .OR.
     *      (ISTOK.EQ.14) .OR. (ISTOK.EQ.18) .OR. (ISTOK.EQ.20) .OR.
     *      (ISTOK.EQ.21) .OR. (ISTOK.EQ.NSTOK)) PFLAGS(1) = T
         IF ((ISTOK.EQ.1) .OR. ((ISTOK.GE.5) .AND. (ISTOK.LE.7)) .OR.
     *      (ISTOK.EQ.15) .OR. (ISTOK.EQ.18) .OR. (ISTOK.EQ.NSTOK) .OR.
     *      (ISTOK.EQ.20) .OR. (ISTOK.EQ.21)) PFLAGS(2) = T
         IF ((ISTOK.EQ.16) .OR. (ISTOK.EQ.19) .OR.
     *      ((ISTOK.GE.21) .AND. (ISTOK.LE.NSTOK))) PFLAGS(3) = T
         IF ((ISTOK.EQ.17) .OR. (ISTOK.EQ.19) .OR.
     *      ((ISTOK.GE.21) .AND. (ISTOK.LE.NSTOK))) PFLAGS(4) = T
         END IF
      GO TO 999
C                                       Data incompatible
 950  WRITE (MSGTXT,1950)
      IRET = 1
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('FLGSTK: UNKNOWN STOKES PARAMETER: ',A4)
 1070 FORMAT ('FLGSTK: UNKNOWN OPCODE: ',A4)
 1950 FORMAT ('FLGSTK: DATA INCOMPATIBLE WITH REQUEST')
      END

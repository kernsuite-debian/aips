      SUBROUTINE VISCHK (OPCODE, STYPE, JADR, VISIN, WT, VISOUT, IERR)
C-----------------------------------------------------------------------
C! checks if UV data sample is desired, returns it in RR, LL, RL, LR
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   VISCHK is used with AIPS map making type operations to determine
C   if the uv sample is desired and to return the sample in the
C   normal set (RR, LL, RL, LR).
C   Inputs:
C      OPCODE   C*4       'INIT' inits JADR, checks data set type
C                         'WANT' determines if vis sample wanted
C                         'GETV' does WANT, returns RR et al.
C      STYPE    I         Stokes mapping type: 1 - 7 => I, IQU, IV,
C                         IQUV, R, L, RL
C      VISIN    R(*)      Visibilities from data record (GETV)
C   In/Out:
C      JADR     I(4)      Pointers to pick up vis data: set by INIT,
C                         used by WANT and GETV.
C   Output:
C      WT       R(4)      Weight array (GETV)
C      VISOUT   R(2,4)    Visibilities in RR, LL, RL, LR (GETV)
C      IERR     I         Error code: 0 => ok
C                            1 => bad OPCODE specified
C                            2 => Stokes req. impossible (INIT)
C                            3 => Vis sample not wanted
C   Common:
C      /MAPHDR/ input     CATBLK gives coordinate info
C      /UVHDR/  input     UV pointers set by UVPGET
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   STYPE, JADR(4), IERR
      REAL      VISIN(*), WT(4), VISOUT(8)
C
      REAL      TEMP, RWT, CATR(256)
      INTEGER   I, J, IROUND, IT, K
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATR
C-----------------------------------------------------------------------
C                                       INIT
      IF (OPCODE.EQ.'INIT') THEN
         IERR = 0
         CALL FILL (4, 0, JADR)
C                                       Real Stokes
         IF (ICOR0.GT.0) THEN
            DO 10 I = 1,4
               TEMP = (I-ICOR0) / CATR(KRCIC+JLOCS) + 1.0
               IT = IROUND(TEMP)
               IF ((ABS(TEMP-IT).LT.0.1) .AND. (IT.GE.1) .AND.
     *            (IT.LE.NCOR)) JADR(IT) = (I-1) * INCS + 1
 10            CONTINUE
C                                       Check real Stokes
            IF (JADR(1).LE.0) GO TO 990
            IF ((STYPE.GT.2) .AND. (JADR(4).LE.0)) GO TO 990
            IF (((STYPE.EQ.2) .OR. (STYPE.EQ.4)) .AND. ((JADR(2).LE.0)
     *         .OR. (JADR(3).LE.0))) GO TO 990
C                                       Init RR ...
         ELSE
            DO 30 I = 1,4
               TEMP = -(I+ICOR0) / CATR(KRCIC+JLOCS) + 1.0
               IT = IROUND(TEMP)
               IF ((ABS(TEMP-IT).LT.0.1) .AND. (IT.GE.1) .AND.
     *            (IT.LE.NCOR)) JADR(IT) = (I-1) * INCS + 1
 30            CONTINUE
C                                       Check RR Stokes
            IF ((JADR(1).LE.0) .AND. (JADR(2).LE.0)) GO TO 990
            IF (((STYPE.EQ.3) .OR. (STYPE.EQ.4) .OR. (STYPE.EQ.7)) .AND.
     *         ((JADR(1).LE.0) .OR. (JADR(2).LE.0))) GO TO 990
            IF ((STYPE.EQ.5) .AND. (JADR(1).LE.0)) GO TO 990
            IF ((STYPE.EQ.6) .AND. (JADR(2).LE.0)) GO TO 990
            IF (((STYPE.EQ.2) .OR. (STYPE.EQ.4)) .AND. ((JADR(3).LE.0)
     *         .OR. (JADR(4).LE.0))) GO TO 990
            END IF
C                                       WANT this one?
      ELSE IF ((OPCODE.EQ.'WANT') .OR. (OPCODE.EQ.'GETV')) THEN
         IERR = 3
         IF (ICOR0.GT.0) THEN
            IF (VISIN(JADR(1)+2).LE.0.) GO TO 999
            IF ((STYPE.GT.2) .AND. (VISIN(JADR(4)+2).LE.0.)) GO TO 999
            IF (((STYPE.EQ.2) .OR. (STYPE.EQ.4)) .AND.
     *         (((VISIN(JADR(2)+2).LE.0.) .AND. (JADR(2).GT.0)) .OR.
     *         ((VISIN(JADR(3)+2).LE.0.) .AND. (JADR(3).GT.0))))
     *         GO TO 999
         ELSE
            IF ((JADR(1).GT.0) .AND. (JADR(2).GT.0) .AND.
     *         (VISIN(JADR(1)+2).LE.0.) .AND. (VISIN(JADR(2)+2).LE.0.))
     *         GO TO 999
            IF (((STYPE.EQ.3) .OR. (STYPE.EQ.4) .OR. (STYPE.EQ.7)) .AND.
     *         ((VISIN(JADR(1)+2).LE.0.) .OR. (VISIN(JADR(2)+2).LE.0.)))
     *         GO TO 999
            IF ((STYPE.EQ.5) .AND. (VISIN(JADR(1)+2).LE.0)) GO TO 999
            IF ((STYPE.EQ.6) .AND. (VISIN(JADR(2)+2).LE.0)) GO TO 999
            IF (((STYPE.EQ.2) .OR. (STYPE.EQ.4)) .AND.
     *         ((VISIN(JADR(3)+2).LE.0.) .OR. (VISIN(JADR(4)+2).LE.0.)))
     *         GO TO 999
            END IF
C                                       do GETVis part
         IERR = 0
         IF (OPCODE.EQ.'GETV') THEN
            CALL RFILL (4, 0.0, WT)
C                                       RR type -> correct RR order
            IF (ICOR0.LE.0) THEN
               DO 210 I = 1,4
                  J = JADR(I)
                  IF (J.GT.0) THEN
                     WT(I) = MAX (0.0, VISIN(J+2))
                     VISOUT(2*I-1) = VISIN(J)
                     VISOUT(2*I) = VISIN(J+1)
                     END IF
 210              CONTINUE
C                                       True Stokes -> RR type
 220        ELSE
               J = JADR(1)
               RWT = 0.0
               IF ((J.GT.0) .AND. (VISIN(J+2).GT.0.)) THEN
                  RWT = VISIN(J+2)
                  VISOUT(1) = VISIN(J)
                  VISOUT(2) = VISIN(J+1)
                  VISOUT(3) = VISOUT(1)
                  VISOUT(4) = VISOUT(2)
                  J = JADR(4)
                  IF ((J.GT.0) .AND. (VISIN(J+2).GT.0.)) THEN
                     VISOUT(1) = VISOUT(1) + VISIN(J)
                     VISOUT(2) = VISOUT(2) + VISIN(J+1)
                     VISOUT(3) = VISOUT(3) - VISIN(J)
                     VISOUT(4) = VISOUT(4) - VISIN(J+1)
                     RWT = MIN (RWT, VISIN(J+2))
                     END IF
                  END IF
 230           J = JADR(2)
               K = JADR(3)
               IF ((J.GT.0) .AND. (K.GT.0) .AND. (VISIN(J+2).GT.0.)
     *            .AND. (VISIN(K+2).GT.0.)) THEN
                  VISOUT(5) = VISIN(J) - VISIN(K+1)
                  VISOUT(6) = VISIN(J+1) + VISIN(K)
                  VISOUT(7) = VISIN(J) + VISIN(K+1)
                  VISOUT(8) = VISIN(J+1) - VISIN(K)
                  IF (RWT.EQ.0) RWT = VISIN(J+2)
                  RWT = MIN (RWT, VISIN(J+2))
                  RWT = MIN (RWT, VISIN(K+2))
                  END IF
               CALL RFILL (4, RWT, WT)
               END IF
            END IF
C                                       bad opcode
      ELSE
         IERR = 1
         END IF
      GO TO 999
C                                       Error report on data set
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('REQUESTED STOKES NOT AVAILABLE IN DATA SET')
      END

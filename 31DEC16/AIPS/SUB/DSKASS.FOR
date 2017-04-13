      SUBROUTINE DSKASS (IOP, RHOST, IERR)
C-----------------------------------------------------------------------
C! assign and de-assign disks for AIPS by host
C# POPS-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2011
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
C   DSKASS will add to the DA0n: assignments or remove one as needed
C   Inputs:
C      IOP     I       1 - assign, 2 de-assign
C      RHOST   C*(*)   Remote host to be added or removed
C   Output
C      IERR    I       Error code: 0 good
C-----------------------------------------------------------------------
      INTEGER   IOP, IERR
      CHARACTER RHOST*(*)
C
      INTEGER   I, I1, I2, J, JJ, JTRIM, TXLUN, TXIND, SCRBUF(256),
     *   INVOL
      CHARACTER DVAL*4, XLATED*256
      HOLLERITH HVAL, HNAME(48)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      INCLUDE 'INCS:DISK.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      TXIND = 0
C                                       assign
      IF (IOP.EQ.1) THEN
         INVOL = NVOL
         CALL ZTRLOG (11, 'DADEVS_FILE', 256, XLATED, JJ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'TRANSLATING DADEVS_FILE LOGICAL'
            GO TO 990
            END IF
         TXLUN = 11
         CALL ZTXOPN ('QRED', TXLUN, TXIND, XLATED(:JJ), .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN DADEVS FILE'
            GO TO 990
            END IF
         JJ = JTRIM (RHOST)
         I1 = 0
 110     CALL ZTXIO ('READ', TXLUN, TXIND, XLATED, IERR)
         IF (IERR.EQ.2) THEN
            CALL ZTXCLS (TXLUN, TXIND, IERR)
            TXIND = 0
         ELSE IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING DADEVS FILE'
            GO TO 990
         ELSE IF (IERR.EQ.0) THEN
            IF (XLATED(:3).NE.'-  ') GO TO 110
            I = INDEX (XLATED(4:), RHOST(:JJ))
            IF (I.GT.0) THEN
               I = JTRIM (XLATED)
               CALL ZEHEX (NVOL+1, 4, DVAL)
               DVAL(:3) = 'DA0'
               CALL CHR2H (4, DVAL, 1, HVAL)
               I2 = I - 3
               CALL CHR2H (I2, XLATED(4:I), 1, HNAME)
               CALL ZCRLOG (4, HVAL, I2, HNAME, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'CREATE LOGICAL '
     *               // DVAL
                  GO TO 990
                  END IF
               NVOL = NVOL + 1
               DSKHST(NVOL) = RHOST
               DSKNAM(NVOL) = XLATED(4:I)
               MSGSUP = 32000
               CALL CATCR (NVOL, 50, SCRBUF, IERR)
               MSGSUP = 0
               IF (IERR.NE.0) THEN
                  MSGTXT = XLATED(4:I) // ' NOT AVAILABLE'
                  CALL MSGWRT (6)
                  DSKHST(NVOL) = ' '
                  DSKNAM(NVOL) = ' '
                  NVOL = NVOL - 1
                  IERR = 0
                  END IF
               END IF
            GO TO 110
            END IF
         CALL ZDRCHK
         CALL GNETSP (INVOL+1, NVOL, DSKNAM)
C                                       de-assign
      ELSE
         I1 = 0
         DO 210 I = MINDSK+1,NVOL
            IF (RHOST.EQ.DSKHST(I)) THEN
               I1 = I1 + 1
               DSKHST(I) = ' '
               DSKNAM(I) = ' '
               END IF
 210        CONTINUE
C                                       re-assign needed
         IF (I1.GT.0) THEN
            IF (NVOL-I1.GT.MINDSK) THEN
               DO 220 I = MINDSK+1,NVOL
                  IF (DSKHST(I).EQ.' ') THEN
                     DO 215 J = I+1,NVOL
                        IF (DSKHST(J).NE.' ') THEN
                           DSKHST(I) = DSKHST(J)
                           DSKNAM(I) = DSKNAM(J)
                           TIMEDA(I) = TIMEDA(J)
                           CALL COPY (8, DASSGN(1,J), DASSGN(1,I))
                           DSKLOK(I) = DSKLOK(J)
                           DSKHST(J) = ' '
                           DSKNAM(J) = ' '
                           CALL ZEHEX (I, 4, DVAL)
                           DVAL(:3) = 'DA0'
                           JJ = JTRIM (DSKNAM(I))
                           CALL CHR2H (4, DVAL, 1, HVAL)
                           CALL CHR2H (JJ, DSKNAM(I), 1, HNAME)
                           CALL ZCRLOG (4, HVAL, JJ, HNAME, IERR)
                           IF (IERR.NE.0) THEN
                              WRITE (MSGTXT,1000) IERR, 'RESET LOGICAL '
     *                           // DVAL
                              GO TO 990
                              END IF
                           GO TO 220
                           END IF
 215                    CONTINUE
                     END IF
 220              CONTINUE
               END IF
            DO 230 I = NVOL-I1+1,NVOL
               CALL ZEHEX (I, 4, DVAL)
               DVAL(:3) = 'DA0'
               CALL CHR2H (4, DVAL, 1, HVAL)
               CALL CHR2H (4, DSKNAM(I), 1, HNAME)
               CALL ZCRLOG (4, HVAL, 4, HNAME, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'CLEAR LOGICAL ' // DVAL
                  CALL MSGWRT (6)
                  END IF
 230           CONTINUE
            END IF
            NVOL = NVOL - I1
         END IF
      IERR = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSKASS ERROR',I3,' ON ',A)
      END


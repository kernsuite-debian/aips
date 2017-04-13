      SUBROUTINE BATQ (OP, TDELAY, HOST, NQUE, INHUSE, USERID, JOBNO,
     *   BUFFER, IERR)
C-----------------------------------------------------------------------
C! performs operations on batch queue control file such as OPEN RUN CLOS
C# Batch System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999, 2010
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
C   BATQ performs operations on the batch queue control file.  There are
C   64 entries per queue of the form UID, IY/IM/ID, IH/IM/IS, version.
C   The entry status is set by the sign & contents of UID, IY.
C   as UID = 0                     slot empty
C      UID > 0 & IT(1) > 0         job waiting to run
C      UID > 0 & IT(1) < 0         job failed
C      UID < 0 & IT(1) > 0         job running
C      UID < 0 & IT(1) = 0         job being submitted
C      UID < 0 & IT(1) < 0         job finished, slot may be reassigned
C   The last 3 user UID's to have run jobs get lower priority to run
C   again.  Otherwise, the algorithm is simply first in, first out.
C   Inputs:
C      OP       C*4        'OPEN' assigns job # & marks busy
C                          'CLOS' marks done or deletes job #
C                          'RUNN' marks job ready w current time
C                          'FIND' finds next job to run
C                          'WHOO' finds job currently running
C                          'FAIL' sets all running jobs to failed
C      TDELAY   R          Time delay in hours before job should run:
C                             RUNN only
C      NQUE     I          queue number
C      INHUSE   I(3)       user #'s to give lower priority (FIND
C                             uses and resets)
C      USERID   I          user #: OPEN, CLOS, RUNN, WHOO
C      JOBNO    I          job #: CLOS, RUNN
C   Output:
C      USERID   I          user #: FIND
C      JOBNO    I          job #: OPEN, FIND, WHOO
C      BUFFER   I(256)     scratch buffer
C      IERR     I          error code: 0 => ok
C                                      1 => input error
C                                      2 => no room (OPEN)
C                                      3 => no job (FIND,WHOO)
C                                           none running (FAIL)
C                                      4 => disk error
C                                     -1 => job ready later (FIND)
C-----------------------------------------------------------------------
      CHARACTER OP*4, HOST*(*)
      INTEGER   NQUE, INHUSE(3), USERID, JOBNO, BUFFER(256), IERR
      REAL      TDELAY
C
      INTEGER   NOP, IOP, BQLUN, BQIND, IJ, IK, J, IREC, IT(6), ITC(3),
     *   I, IER, II, IL, NWPL, NLPR, NWPR, JTRIM, LK
      DOUBLE PRECISION  JD, JDJ, JDK
      CHARACTER READ*4, WRITE*4, PHNAME*48, BQTYP*2, VERS(4)*4,
     *   OPCODE(6)*4, FILE*64
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      DATA VERS /'OLD:','NEW:','TST:','CVX:'/
      DATA READ, WRITE, T, F /'READ','WRIT',.TRUE.,.FALSE./
      DATA BQTYP, BQLUN /'BQ', 40/
      DATA NOP, OPCODE /6,'OPEN','CLOS','RUNN','FIND','WHOO','FAIL'/
C-----------------------------------------------------------------------
      NWPL = 4
      NLPR = 256 / NWPL
      NWPR = NWPL * NLPR
C                                       check input
      IERR = 1
      DO 10 IOP = 1,NOP
         IF (OP.EQ.OPCODE(IOP)) GO TO 15
 10      CONTINUE
      GO TO 999
C                                       open file & get Q record
 15   IERR = 4
      CALL ZPHFIL (BQTYP, 1, 0, 0, PHNAME, IER)
      J = JTRIM (HOST)
C                                       local
      IF (J.EQ.0) THEN
         FILE = PHNAME
         IF ((NQUE.LT.1) .OR. (NQUE.GT.NBATQS)) GO TO 999
C                                       remote at this location
      ELSE
         FILE = 'NET0:' // HOST(:J) // '/' // PHNAME(6:)
         IF ((NQUE.LT.1) .OR. (NQUE.GT.33)) GO TO 999
         END IF
      CALL ZOPEN (BQLUN, BQIND, 1, FILE, F, T, T, IER)
      IF (IER.NE.0) GO TO 999
      IREC = NQUE
      CALL ZFIO (READ, BQLUN, BQIND, IREC, BUFFER, IER)
      IF (IER.NE.0) GO TO 980
      IERR = 1
C-----------------------------------------------------------------------
C                                       OPEN: assign slot
      IF (OP.EQ.'OPEN') THEN
         IF ((USERID.LT.1) .OR. (USERID.GT.USELIM)) GO TO 980
         IK = 1000
         DO 105 LK = 1,NWPR,NWPL
            IF (BUFFER(LK).EQ.0) IK = MIN (IK, LK)
 105        CONTINUE
         IF (IK.GT.NWPR) THEN
            IJ = 0
            IK = 0
            JDJ = 1.0D12
            JDK = 1.0D12
            IERR = 2
            DO 110 I = 1,NWPR,NWPL
               IF (BUFFER(I+1).LE.0) THEN
                  BUFFER(I+1) = -BUFFER(I+1)
                  CALL CATIME (2, BUFFER(I+1), IT)
                  BUFFER(I+1) = -BUFFER(I+1)
                  CALL DAT2JD (IT, JD)
                  IF (BUFFER(I).LE.0) THEN
                     IF (JD.LE.JDK) THEN
                        JDK = JD
                        IK = I
                        END IF
                  ELSE IF (JD.LE.JDJ) THEN
                     JDJ = JD
                     IJ = I
                     END IF
                  END IF
 110           CONTINUE
            IF (IK.EQ.0) IK = IJ
C                                       IK, IJ = 0 => no room
            IF (IK.EQ.0) GO TO 980
            END IF
C                                       set parms
         JOBNO = (IK-1)/NWPL + 1
         IJ = NWPL*(JOBNO-1) + 1
         BUFFER(IJ) = -USERID
         BUFFER(IJ+1) = 0
         BUFFER(IJ+2) = 0
C                                       set version
         IK = 3
         IF (VERNAM.EQ.VERS(1)) IK = 1
         IF (VERNAM.EQ.VERS(2)) IK = 2
         IF (VERNAM.EQ.VERS(4)) IK = 4
         BUFFER(IJ+3) = IK
C-----------------------------------------------------------------------
C                                       CLOS: delete a slot
      ELSE IF (OP.EQ.'CLOS') THEN
         IF ((JOBNO.LT.1).OR.(JOBNO.GT.NLPR)) GO TO 980
         IK = NWPL * (JOBNO-1) + 1
         IJ = ABS(BUFFER(IK))
         IF (IJ.NE.USERID) GO TO 980
         IF (BUFFER(IK+1).LE.0) THEN
            CALL FILL (NWPL, 0, BUFFER(IK))
C                                        mark done
         ELSE
            BUFFER(IK) = -IJ
            BUFFER(IK+1) = -ABS(BUFFER(IK+1))
            END IF
C-----------------------------------------------------------------------
C                                       RUNN: current time, id -> +
      ELSE IF (OP.EQ.'RUNN') THEN
         IF ((JOBNO.LT.1) .OR. (JOBNO.GT.NLPR)) GO TO 980
         IK = NWPL * (JOBNO-1) + 1
         IJ = ABS (BUFFER(IK))
         IF (IJ.NE.USERID) GO TO 980
         IF ((BUFFER(IK).LT.0) .AND. (BUFFER(IK+1).GT.0)) GO TO 980
         BUFFER(IK) = IJ
         CALL CATIME (1, BUFFER(IK+1), IT)
C                                       Add time delay to submit
         IF (TDELAY.GT.0.0) THEN
            CALL DAT2JD (IT, JD)
            JD = JD + TDELAY / 24.0D0
            CALL JD2DAT (JD, IT)
            IF (IT(1).GT.200) IT(1) = IT(1) - 1900
            BUFFER(IK+1) = 256 * (256 * IT(1) + IT(2)) + IT(3)
            BUFFER(IK+2) = 256 * (256 * IT(4) + IT(5)) + IT(6)
            END IF
C-----------------------------------------------------------------------
C                                       FIND:  a new job to run
C                                       trying to inhibit most
C                                       recent users
      ELSE IF (OP.EQ.'FIND') THEN
         IL = 3
         IF (INHUSE(3).LE.0) IL = 2
         IF (INHUSE(2).LE.0) IL = 1
         IF (INHUSE(1).LE.0) IL = 0
         IK = IL + 2
         DO 430 J = 1,IK
            IJ = 0
            IF (IL.GE.0) THEN
               CALL CATIME (1, ITC, IT)
            ELSE
               ITC(1) = 256 * 256 * 200
               ITC(2) = ITC(1)
               END IF
            DO 425 I = 1,NWPR,NWPL
               IF (BUFFER(I).LE.0) GO TO 425
               IF (BUFFER(I+1).LE.0) GO TO 425
               DO 405 II = 1,IL
                  IF (BUFFER(I).EQ.INHUSE(II)) GO TO 425
 405              CONTINUE
               DO 415 II = 1,2
                  IF (BUFFER(II+I).LT.ITC(II)) GO TO 420
                  IF (BUFFER(II+I).GT.ITC(II)) GO TO 425
 415              CONTINUE
 420           ITC(1) = BUFFER(I+1)
               ITC(2) = BUFFER(I+2)
               IJ = I
 425           CONTINUE
            IF (IJ.NE.0) GO TO 435
            IL = IL - 1
 430        CONTINUE
C                                       none found
         IERR = 3
         GO TO 980
C                                       got one, but for later
 435     IF (IL.LT.0) THEN
            IERR = -1
            GO TO 980
C                                       got one for now
         ELSE
 440        JOBNO = (IJ-1) / NWPL + 1
            USERID = BUFFER(IJ)
            BUFFER(IJ) = -USERID
            INHUSE(3) = INHUSE(2)
            INHUSE(2) = INHUSE(1)
            INHUSE(1) = USERID
            VERNAM = VERS(BUFFER(IJ+3))
            END IF
C-----------------------------------------------------------------------
C                                       WHOO: locate current job
      ELSE IF (OP.EQ.'WHOO') THEN
         IERR = 3
         DO 510 I = 1,NWPR,NWPL
            IF ((BUFFER(I).LT.0) .AND. (BUFFER(I+1).GT.0) .AND.
     *         (USERID.EQ.IABS(BUFFER(I)))) THEN
               IERR = 0
               JOBNO = (I-1) / NWPL + 1
               END IF
 510        CONTINUE
         GO TO 980
C-----------------------------------------------------------------------
C                                       FAIL: all "running" fail
      ELSE IF (OP.EQ.'FAIL') THEN
         IERR = 3
         DO 610 I = 1,NWPR,NWPL
            IF ((BUFFER(I).LT.0) .AND. (BUFFER(I+1).GT.0)) THEN
               IERR = 0
               BUFFER(I) = -BUFFER(I)
               BUFFER(I+1) = -BUFFER(I+1)
               END IF
 610        CONTINUE
         IF (IERR.NE.0) GO TO 980
         END IF
C-----------------------------------------------------------------------
C                                       update Q file
      IERR = 0
      CALL ZFIO (WRITE, BQLUN, BQIND, IREC, BUFFER, IER)
      IF (IER.NE.0) IERR = 4
C                                       close Q file
 980  CALL ZCLOSE (BQLUN, BQIND, IER)
      IF ((IER.NE.0) .AND. (IERR.EQ.0)) IERR = 4
C
 999  RETURN
      END

      SUBROUTINE FIELDF (BOXFIL, NFIELD, MFIELD, RA, DEC, MROTAT,
     *   FLDSIZ, RASH, DECSH, BCOMP, IERR)
C-----------------------------------------------------------------------
C! read field parameters from text file
C# AP Imaging
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003, 2013
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
C   Fills field parameters from the user test file F lines
C   Inputs:
C      BOXFIL   C*48     User provided file name containing box defs
C      NFIELD   I        Number of fields defined for field parms
C      MFIELD   I        Number of fields for BCOMP
C      RA       D        UV data set RA in degrees
C      DEC      D        UV data set Dec in degrees
C      MROTAT   R        Total rotation in degrees CCW from North
C   In/Out:
C      WIN      I(2,*)   image field sizes
C      RASH     R(*)     RA shift
C      DECSH    R(*)     Declination shift
C      BCOMP    I(*)     Subtract initial components 1-BCOMP(i)
C   Outputs:
C      IERR     I        Error return code: 0 => no error
C-----------------------------------------------------------------------
      CHARACTER BOXFIL*48
      INTEGER   NFIELD, MFIELD, FLDSIZ(2,*), BCOMP(*), IERR
      REAL      MROTAT, RASH(*), DECSH(*)
      DOUBLE PRECISION RA, DEC
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN, I, J, FIND, KBP, I1, I2, ICNT(MAXFLD)
      CHARACTER LINE*132, SIGN*1
      DOUBLE PRECISION X, XRA, XDEC
      REAL      TRASH, TDECSH
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      CALL FILL (MAXFLD, 0, ICNT)
C                                        Open clean box file
      LUN = 11
      CALL ZTXOPN ('READ', LUN, FIND, BOXFIL, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                        Enter parameters from file
      I2 = 0
      DO 50 I = 1,500000
         CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
         IF (IERR.EQ.2) GO TO 60
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            CALL MSGWRT (6)
            GO TO 995
            END IF
         CALL CHTRIM (LINE, 132, LINE, J)
C                                       parse for field:
C                                       #, fldsiz, rash, decsh
         IF ((LINE(:1).EQ.'F') .OR. (LINE(:1).EQ.'f')) THEN
            KBP = 2
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) GO TO 50
            IF (X.GE.0.0D0) THEN
               I1 = X + 0.50D0
            ELSE
               I1 = X - 0.50D0
               END IF
            IF ((I1.GE.1) .AND. (I1.LE.NFIELD)) THEN
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               FLDSIZ(1,I1) = X + 0.50D0
               IF (X.LT.0.0D0) FLDSIZ(1,I1) = -1
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               FLDSIZ(2,I1) = X + 0.50D0
               IF (X.LT.0.0D0) FLDSIZ(2,I1) = -1
               CALL GETNUM (LINE, 132, KBP, XRA)
               IF (X.EQ.DBLANK) GO TO 990
               CALL GETNUM (LINE, 132, KBP, XDEC)
               IF (X.EQ.DBLANK) GO TO 990
               ICNT(I1) = ICNT(I1) + 1
               IF (ICNT(I1).GT.1) THEN
                  IF ((ABS(RASH(I1)-XRA).GT.1.D-4) .OR.
     *               (ABS(DECSH(I1)-XDEC).GT.1.D-4)) THEN
                     WRITE (MSGTXT,1010) I1
                     CALL MSGWRT (7)
                     WRITE (MSGTXT,1015) RASH(I1), DECSH(I1),
     *                  XRA, XDEC
                     CALL MSGWRT (7)
                     IERR = 10
                     GO TO 995
                     END IF
                  END IF
               RASH(I1) = XRA
               DECSH(I1) = XDEC
               I2 = I2 + 1
               END IF
C                                       parse for ra/dec field
C                                       # fldsiz ra (HH MM SS) Dec (+-
C                                       DD MM SS)
         ELSE IF ((LINE(:1).EQ.'C') .OR. (LINE(:1).EQ.'c')) THEN
            KBP = 2
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) GO TO 50
            IF (X.GE.0.0D0) THEN
               I1 = X + 0.50D0
            ELSE
               I1 = X - 0.50D0
               END IF
            IF ((I1.GE.1) .AND. (I1.LE.NFIELD)) THEN
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               FLDSIZ(1,I1) = X + 0.50D0
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               FLDSIZ(2,I1) = X + 0.50D0
C                                       RA
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XRA = X
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XRA = XRA + X/60.0D0
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XRA = XRA + X/3600.0D0
               XRA = XRA * 15.0D0
C                                       Sign
               J = KBP
 40            IF (J.GT.132) GO TO 990
               IF (LINE(J:J).EQ.' ') THEN
                  J = J + 1
                  GO TO 40
                  END IF
               SIGN = LINE(J:J)
C                                       DEC
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XDEC = ABS(X)
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XDEC = XDEC + X/60.0D0
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               XDEC = XDEC + X/3600.0D0
               IF (SIGN.EQ.'-') XDEC = -XDEC
C                                       to shifts
               CALL SHFTXY (RA, DEC, MROTAT, XRA, XDEC, TRASH,
     *            TDECSH)
C                                       check
               ICNT(I1) = ICNT(I1) + 1
               IF (ICNT(I1).GT.1) THEN
                  IF ((ABS(RASH(I1)-TRASH).GT.1.D-4) .OR.
     *               (ABS(DECSH(I1)-TDECSH).GT.1.D-4)) THEN
                     WRITE (MSGTXT,1010) I1
                     CALL MSGWRT (7)
                     WRITE (MSGTXT,1015) RASH(I1), DECSH(I1),
     *                  TRASH, TDECSH
                     CALL MSGWRT (7)
                     IERR = 10
                     GO TO 995
                     END IF
                  END IF
C                                       to shifts
               RASH(I1) = TRASH
               DECSH(I1) = TDECSH
               I2 = I2 + 1
               END IF
C                                       #, BCOMP(#)
         ELSE IF ((LINE(:1).EQ.'B') .OR. (LINE(:1).EQ.'b')) THEN
            KBP = 2
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) GO TO 50
            IF (X.GE.0.0D0) THEN
               I1 = X + 0.50D0
            ELSE
               I1 = X - 0.50D0
               END IF
            IF ((I1.GE.1) .AND. (I1.LE.MFIELD)) THEN
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 990
               BCOMP(I1) = X + 0.50D0
               I2 = I2 + 1
               END IF
            END IF
 50      CONTINUE
 60   CALL ZTXCLS (LUN, FIND, I)
      IERR = 0
C
      IF (I2.GT.0) THEN
         IERR = -I2
         MSGTXT = 'FIELDF: Final field parameters and # read are:'
         CALL MSGWRT (3)
         DO 80 J = 1,NFIELD
            WRITE (MSGTXT,1070) 'Field', J, FLDSIZ(1,J), FLDSIZ(2,J),
     *         RASH(J), DECSH(J), BCOMP(J)
            CALL MSGWRT (3)
 80         CONTINUE
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) I
      CALL MSGWRT (6)
      IERR = 1
 995  CALL ZTXCLS (LUN, FIND, I)

 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIELDF: ERROR',I4,1X,A,'ING BOXFILE')
 1010 FORMAT ('FIELDF: FACET',I5,' BOXFILE HAS 2 COORDINATE CARDS;')
 1015 FORMAT ('        SHIFTS',2F11.3,2X,2F11.3)
 1070 FORMAT (A5,I5,2I5,2(1PE12.4),I11)
 1990 FORMAT ('FIELDF: PARSING ERROR ON LINE',I4)
      END

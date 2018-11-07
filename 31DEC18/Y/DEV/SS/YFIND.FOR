      SUBROUTINE YFIND (MAXPL, TYPE, IPL, UNIQUE, CATBLK, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! determines the unique TV image of desired type, returns catalog block
C# Y0 TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1996
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   YFIND determines which of the visible TV images the user wishes
C   to select.  The TV must already be open.
C   Inputs:
C      MAXPL   I        Highest plane number allowed (i.e. do graphics
C                       count?)
C      TYPE    C*2      2-char image type to restrict search
C   Output:
C      IPL     I        Plane number found
C      UNIQUE  L        T => only one image visible now
C                           (all types except zeroed ones ('ZZ'))
C      CATBLK  I(256)   Image catalog block found
C      SCRTCH  I(256)   Scratch buffer
C      IERR    I        Error code: 0 => ok
C                          1 => no image
C                          2 => IO error in image catalog
C                          3 => TV error
C                         10 => > 1 image of requested type
C   Generic Y routine: for local host-controlled TV device
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      LOGICAL   UNIQUE
      INTEGER   SCRTCH(256), MAXPL, IPL, CATBLK(256), IERR
C
      CHARACTER CTEST*2
      INTEGER   ONPL, ZOR, ILUN, IIND, IER, IC, NLAST, I, J, K, KPL, LL,
     *   ITC, IOFF, I2TMP1, ZAND, IREC, NR, NR0, DAT(4), OPCODE,
     *   WORK(260), NWSD
      LOGICAL   ANY, EQUAL, ISZZ
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
      IERR = 1
C                                       Which planes are on ?
      UNIQUE = .TRUE.
      ONPL = 0
      DO 10 I = 1,4
         ONPL = ZOR (ONPL, TVLIMG(I))
 10      CONTINUE
      IF (ONPL.EQ.0) GO TO 999
      IERR = 0
      IOFF = KHPTY
      ANY = TYPE.EQ.'  '
      ISZZ = TYPE.EQ.'ZZ'
C                                       Look in image catalog: is it
C                                       only one image visible
C                                       DISK
      IF (TVIMGC.LE.0) THEN
         CALL ICOPEN (1, ILUN, IIND, IER)
         IF (IER.NE.0) THEN
            IERR = 2
            GO TO 999
            END IF
         IC = 0
         ITC = 0
         LL = (NIMAGE - 1) / 51
         DO 40 I = 1,MAXPL
            I2TMP1 = 2 ** (I-1)
            IF (ZAND(ONPL, I2TMP1).NE.0) THEN
C                                       Multi image plane
               IF (I.LE.NGRAY) THEN
                  NR = 1 + (I-1) * (1+NIMAGE+LL)
                  NR0 = NR
                  CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
                  IF (IER.NE.0) GO TO 960
                  NLAST = SCRTCH(1)
                  IF (NLAST.GT.0) THEN
                     DO 20 J = 1,NIMAGE
                        IF ((J.NE.1) .AND. (MOD(J,51).EQ.1)) THEN
                           NR = NR + 1
                           CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH,
     *                        IER)
                           IF (IER.NE.0) GO TO 960
                           END IF
                        K = 5 * MOD (J-1, 51) + 2
                        IF ((SCRTCH(K).GT.0) .AND. (SCRTCH(K).LE.NLAST))
     *                     THEN
                           ITC = ITC + 1
                           IF (.NOT.ANY) THEN
                              IREC = J + NR0 + LL
                              CALL ZFIO ('READ', ILUN, IIND, IREC,
     *                           CATBLK, IER)
                              IF (IER.NE.0) GO TO 960
                              CALL H2CHR (2, KHPTYO, CATBLK(IOFF),
     *                           CTEST)
                              EQUAL = CTEST.EQ.'ZZ'
                              IF ((EQUAL) .AND. (.NOT.ISZZ))
     *                           ITC = ITC - 1
                              IF (TYPE.NE.CTEST) GO TO 20
                              END IF
                           IF (IC.GE.1) GO TO 50
                           IC = IC + 1
                           IPL = I
                           KPL = J + NR0 + LL
                           END IF
 20                     CONTINUE
                     END IF
C                                       Single image planes
               ELSE
                  NR = I + NGRAY * (NIMAGE + LL)
                  CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
                  IF (IER.NE.0) GO TO 960
                  IF ((SCRTCH(IICOR).GT.0) .AND. (SCRTCH(IICOR+1).GT.0)
     *               .AND. (SCRTCH(IICOR+2).GT.SCRTCH(IICOR))) THEN
                     ITC = ITC + 1
                     IF (.NOT.ANY) THEN
                        IREC = NR
                        CALL H2CHR (2, KHPTYO, CATBLK(IOFF), CTEST)
                        IF (TYPE.NE.CTEST) GO TO 40
                        END IF
                     IF (IC.GE.1) GO TO 50
                     IC = IC + 1
                     IPL = I
                     KPL = NR
                     END IF
                  END IF
               END IF
 40         CONTINUE
C                                       No image actually on
         IF (IC.LE.0) THEN
            IERR = 1
C                                       Unique: get header
         ELSE
            IREC = KPL
            CALL ZFIO ('READ', ILUN, IIND, IREC, CATBLK, IER)
            IF (IER.NE.0) GO TO 960
            UNIQUE = ITC.EQ.1
            END IF
         GO TO 990
C                                       Not unique: ask user
 50      UNIQUE = .FALSE.
         IERR = 10
         GO TO 990
C                                       From XAS image catalog
      ELSE
         OPCODE = 35
         DAT(1) = MAXPL
         CALL FILL (3, 0, DAT(2))
         IF (.NOT.ANY) DAT(2) = 1
         CALL ZCLC8 (2, TYPE, 1, WORK)
         CALL ZSSSXF (OPCODE, DAT, 2, WORK, NWSD, IERR)
         IF (IERR.EQ.0) THEN
            CALL CATN2L ('N2LI', WORK, CATBLK)
            CALL ZI8IL (3, 1025, WORK, SCRTCH)
            IPL = SCRTCH(1)
            UNIQUE = SCRTCH(2).GT.0
            IERR = SCRTCH(3)
         ELSE
            IERR = 2
            WRITE (MSGTXT,1200) IERR
            CALL MSGWRT (6)
            END IF
         GO TO 999
         END IF
C                                       Errors
 960  IERR = 2
C
 990  CALL ZCLOSE (ILUN, IIND, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('YCOVER: XAS IMAGE CATALOG RETURNS ERROR',I5)
      END

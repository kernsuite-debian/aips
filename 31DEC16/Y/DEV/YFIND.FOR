      SUBROUTINE YFIND (MAXPL, TYPE, IPL, UNIQUE, CATBLK, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! determines the unique TV image of desired type, returns catalog block
C# Y0 TV-util
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
     *   ITC, IOFF, I2TMP1, ZAND, IREC, NR, NR0
      LOGICAL   ANY, EQUAL, ISZZ
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
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
      CALL ICOPEN (1, ILUN, IIND, IER)
      IF (IER.EQ.0) GO TO 15
         IERR = 2
         GO TO 999
 15   IC = 0
      ITC = 0
      LL = (NIMAGE - 1) / 51
      DO 40 I = 1,MAXPL
         I2TMP1 = 2 ** (I-1)
         IF (ZAND(ONPL, I2TMP1).EQ.0) GO TO 40
C                                       Multi image plane
         IF (I.GT.NGRAY) GO TO 30
            NR = 1 + (I-1) * (1+NIMAGE+LL)
            NR0 = NR
            CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
            IF (IER.NE.0) GO TO 960
            NLAST = SCRTCH(1)
            IF (NLAST.LE.0) GO TO 40
            DO 20 J = 1,NIMAGE
               IF ((J.EQ.1) .OR. (MOD(J,51).NE.1)) GO TO 16
                  NR = NR + 1
                  CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
                  IF (IER.NE.0) GO TO 960
 16            K = 5 * MOD (J-1, 51) + 2
               IF ((SCRTCH(K).LE.0) .OR. (SCRTCH(K).GT.NLAST)) GO TO 20
                  ITC = ITC + 1
                  IF (ANY) GO TO 18
                     IREC = J + NR0 + LL
                     CALL ZFIO ('READ', ILUN, IIND, IREC, CATBLK, IER)
                     IF (IER.NE.0) GO TO 960
                     CALL H2CHR (2, KHPTYO, CATBLK(IOFF), CTEST)
                     EQUAL = CTEST.EQ.'ZZ'
                     IF ((EQUAL) .AND. (.NOT.ISZZ)) ITC = ITC - 1
                     IF (TYPE.NE.CTEST) GO TO 20
 18               IF (IC.GE.1) GO TO 50
                  IC = IC + 1
                  IPL = I
                  KPL = J + NR0 + LL
 20            CONTINUE
            GO TO 40
C                                       Single image planes
 30      CONTINUE
            NR = I + NGRAY * (NIMAGE + LL)
            CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
            IF (IER.NE.0) GO TO 960
            IF ((SCRTCH(IICOR).LE.0) .OR. (SCRTCH(IICOR+1).LE.0) .OR.
     *         (SCRTCH(IICOR+2).LE.SCRTCH(IICOR))) GO TO 40
               ITC = ITC + 1
               IF (ANY) GO TO 35
                  IREC = NR
                  CALL H2CHR (2, KHPTYO, CATBLK(IOFF), CTEST)
                  IF (TYPE.NE.CTEST) GO TO 40
 35            IF (IC.GE.1) GO TO 50
               IC = IC + 1
               IPL = I
               KPL = NR
 40      CONTINUE
C                                       No image actually on
      IF (IC.GT.0) GO TO 45
         CALL ZCLOSE (ILUN, IIND, IER)
         IERR = 1
         GO TO 999
C                                       Unique: get header
 45   IREC = KPL
      CALL ZFIO ('READ', ILUN, IIND, IREC, CATBLK, IER)
      IF (IER.NE.0) GO TO 960
      CALL ZCLOSE (ILUN, IIND, IER)
      UNIQUE = ITC.EQ.1
      GO TO 999
C                                       Not unique: ask user
 50   CALL ZCLOSE (ILUN, IIND, IER)
      UNIQUE = .FALSE.
      IERR = 10
      GO TO 999
C                                       Errors
 960  CALL ZCLOSE (ILUN, IIND, IER)
      IERR = 2
C
 999  RETURN
      END

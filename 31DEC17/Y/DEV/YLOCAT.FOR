      SUBROUTINE YLOCAT (NP, XP, YP, NAME, CLASS, SEQ, DISK, PTYP, IX,
     *   IY, IQ, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! return TV positions for set of image positions
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
C   YLOCAT locates a set of image pixel positions on the TV for a
C   specified image using only those grey planes that are turned on.
C   Inputs:
C      NP      I         Number of pixel positions
C      XP      R(NP)     X image pixel positions
C      YP      R(NP)     Y image pixel positions
C      NAME    C*12      Packed image name (name) '    ' => any
C      CLASS   C*6       Packed image name (class) '    ' => any
C      SEQ     I         Image name (sequence #)  0 => any
C      DISK    I         Image file disk 0 => any
C      PTYP    C*2       Image type  '  ' => any
C   Output:
C      IX      I(NP)     TV x positions
C      IY      I(NP)     TV y positions
C      IQ      I(NP)     TV channels  0 => none this position
C      IERR    I         Error code: 0 -> ok
C                           2 -> input error
C                           3 -> IO error
C                          11 -> some positions bad
C                          12 -> no positions found
C   Uses common /MAPHDR/ results unpredictable except on IERR = 0
C   then = image catlg header of last position found.
C   GENERIC Y ROUTINE: for local host-operated TV device
C-----------------------------------------------------------------------
      CHARACTER NAME*12, CLASS*6, PTYP*2
      INTEGER   NP, SEQ, DISK, IX(*), IY(*), IQ(*), SCRTCH(256), IERR
      REAL      XP(*), YP(*)
C
      INTEGER   IREC, NR, ONPL, ZOR, ZAND, ILUN, IIND, IER, LL, NF, IP,
     *   NLAST, JI, I, K, J, NR0, NPAT(12), CPAT(6), JP
      LOGICAL   LNAME, LCLASS, LPTYP, EQUAL
      CHARACTER CTEST*2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA ILUN /20/
C-----------------------------------------------------------------------
C                                       planes that are on
      IERR = 2
      IF (NP.LE.0) GO TO 999
      ONPL = 0
      DO 10 I = 1,4
         ONPL = ZOR (ONPL, TVLIMG(I))
 10      CONTINUE
      IF (ONPL.EQ.0) GO TO 999
C                                       what to test
      LNAME = NAME.EQ.' '
      LCLASS = CLASS.EQ.' '
      LPTYP = PTYP.EQ.' '
      IF (.NOT.LNAME) CALL PSFORM (12, NAME, NPAT)
      IF (.NOT.LCLASS) CALL PSFORM (6, CLASS, CPAT)
C                                       open image catalog
      IERR = 3
      CALL ICOPEN (1, ILUN, IIND, IER)
      IF (IER.EQ.0) GO TO 15
         WRITE (MSGTXT,1010) IER
         GO TO 990
 15   LL = (NIMAGE - 1) / 51
      NF = 0
      CALL FILL (NP, 0, IX)
      CALL FILL (NP, 0, IY)
      CALL FILL (NP, 0, IQ)
C                                       loop over planes
      DO 50 IP = 1,NGRAY
         JP = 2 ** (IP - 1)
         IF (ZAND(ONPL,JP).EQ.0) GO TO 50
C                                       read plane dir rec # 1
         NR = 1 + (IP - 1) * (1 + LL + NIMAGE)
         NR0 = NR
         CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
         IF (IER.NE.0) GO TO 960
         NLAST = SCRTCH(1)
         IF (NLAST.LE.0) GO TO 50
C                                       loop over images in plane
         DO 40 JI = 1,NIMAGE
C                                       read another dir record
            IF ((JI.EQ.1) .OR. (MOD(JI,51).NE.1)) GO TO 20
               NR = NR + 1
               CALL ZFIO ('READ', ILUN, IIND, NR, SCRTCH, IER)
               IF (IER.NE.0) GO TO 960
 20         K = 5 * MOD (JI-1, 51) + 2
            IF ((SCRTCH(K).LE.0) .OR. (SCRTCH(K).GT.NLAST)) GO TO 40
C                                       read actual img cat block
            IREC = JI + NR0 + LL
            CALL ZFIO ('READ', ILUN, IIND, IREC, CATBLK, IER)
            IF (IER.NE.0) GO TO 960
C                                       test name parms
            EQUAL = LNAME
            IF (.NOT.EQUAL) CALL CHWMAT (12, NAME, NPAT, KHIMNO,
     *         CATH(KHIMN), EQUAL)
            IF (.NOT.EQUAL) GO TO 40
            EQUAL = LCLASS
            IF (.NOT.EQUAL) CALL CHWMAT (6, CLASS, CPAT, KHIMCO,
     *         CATH(KHIMC), EQUAL)
            IF (.NOT.EQUAL) GO TO 40
            EQUAL = LPTYP
            IF (.NOT.EQUAL) THEN
               CALL H2CHR (2, KHPTYO, CATH(KHPTY), CTEST)
               EQUAL = PTYP.EQ.CTEST
               END IF
            IF (.NOT.EQUAL) GO TO 40
            IF ((SEQ.GT.0) .AND. (CATBLK(KIIMS).NE.SEQ)) GO TO 40
            IF ((DISK.GT.0) .AND. (CATBLK(IIVOL).NE.DISK)) GO TO 40
C                                       a desired image: loop points
            DO 30 I = 1,NP
C                                       get unfilled positions if in
C                                       subimage area
               IF (IQ(I).GT.0) GO TO 30
                  J = XP(I) + 0.5
                  IF ((J.LT.CATBLK(IIWIN)) .OR. (J.GT.CATBLK(IIWIN+2)))
     *               GO TO 30
                  J = YP(I) + 0.5
                  IF ((J.LT.CATBLK(IIWIN+1)) .OR.
     *               (J.GT.CATBLK(IIWIN+3))) GO TO 30
                     NF = NF + 1
                     IX(I) = CATBLK(IICOR)
                     IY(I) = CATBLK(IICOR+1)
                     IF (CATBLK(IIWIN+2).NE.CATBLK(IIWIN)) IX(I) =
     *                  IX(I) + (XP(I) - CATBLK(IIWIN)) *
     *                  REAL (CATBLK(IICOR+2) - CATBLK(IICOR)) /
     *                  REAL (CATBLK(IIWIN+2) - CATBLK(IIWIN))
                     IF (CATBLK(IIWIN+3).NE.CATBLK(IIWIN+1)) IY(I) =
     *                  IY(I) + (YP(I) - CATBLK(IIWIN+1)) *
     *                  REAL (CATBLK(IICOR+3) - CATBLK(IICOR+1)) /
     *                  REAL (CATBLK(IIWIN+3) - CATBLK(IIWIN+1))
                     IQ(I) = IP
                     IF (NF.GE.NP) GO TO 60
 30            CONTINUE
 40         CONTINUE
 50      CONTINUE
C                                       all through
 60   CALL ZCLOSE (ILUN, IIND, IER)
      IERR = 12
      IF (NF.GT.0) IERR = 11
      IF (NF.GE.NP) IERR = 0
      GO TO 999
C                                       error in IO
 960  WRITE (MSGTXT,1960) IER
      CALL ZCLOSE (ILUN, IIND, IER)
C
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('YLOCAT: CANNOT OPEN IMAGE CATALOG',I7)
 1960 FORMAT ('YLOCAT: IMAGE CATALOG READ ERROR',I7)
      END

LOCAL INCLUDE 'LAYER.INC'
C                                       inputs
      HOLLERITH XNAM(3), XCLS(2)
      REAL      XSEQ, XDISK, BLC(7), TRC(7), APARM(10), BPARM(10),
     *   CPARM(10), DPARM(10), EPARM(10), PLCOLR(3,10), DOCOLR, DOWT
      COMMON /INPARM/ XNAM, XCLS, XSEQ, XDISK, BLC, TRC, APARM, BPARM,
     *   CPARM, DPARM, EPARM, PLCOLR, DOCOLR, DOWT
      CHARACTER INFILE*36, OUFILE*36, OUFIL2*36
      COMMON /LAYERC/ INFILE, OUFILE, OUFIL2
C                                       headers
      REAL      NEWR(256), OLDR(256), WRKR(256)
      INTEGER   NEWI(256), OLDI(256), WRKI(256)
      HOLLERITH NEWH(256), OLDH(256), WRKH(256)
      DOUBLE PRECISION NEWD(128), OLDD(128), WRKD(128)
      EQUIVALENCE (NEWR, NEWI, NEWD, NEWH)
      EQUIVALENCE (OLDR, OLDI, OLDD, OLDH)
      EQUIVALENCE (WRKR, WRKI, WRKD, WRKH)
      INCLUDE 'INCS:PMAD.INC'
      REAL      BUFF1(MABFSS), BUFF2(MABFSS)
      INTEGER   AX, NROW
      COMMON /LAYERV/ NEWI, OLDI, WRKI, BUFF1, BUFF2, AX, NROW
LOCAL END
      PROGRAM LAYER
C-----------------------------------------------------------------------
C! Makes red-green-blue image from input images.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2008
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
C  Program LAYER will produce a cubed image with 3 planes, each
C  representing one of the colors red, green, and blue.  Each input
C  image makes a weighted contribution to each of the planes of the
C  output image according to the user's input adverbs.
C  Inputs:  Adverbs from AIPS
C     OUTNAME   R(3)   Name of output image.
C     OUTCLASS  R(2)   Class of the output image.
C     OUTSEQ    R   Sequence number of the output image.
C     OUTDISK   R   Disk number for output image.
C     BLC       R(7)   Bottom left corner of input cube. 0=> 1,1,1
C     TRC       R(7)   Top right corner of input cube. 0=> max.
C     APARM     I(10)  disk numbers
C     BPARM     I(10)  catalog numbers
C     CPARM     R(10)  absorption factors
C     DPARM     R(10)  emission factors
C     PLCOLORS  R(3,10) color factors
C     DOCOLOR   L      absorption colored?
C     DOWEIGHT  L      emission weighted?
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   IERR, INLUN, IOTLUN, IOTLU2, IHINL, IHOTL, I, ID, IS,
     *   IDG, ISG
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'LAYER.INC'
      DATA PRGNAM /'LAYER '/
      DATA INLUN, IOTLUN, IOTLU2, IHINL, IHOTL /16,17,18, 27,28/
C-----------------------------------------------------------------------
C                                       Set up, get parms, open, create
C                                       all files.
      CALL SETUP (PRGNAM, INLUN, IOTLUN, IOTLU2, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Add in an image
      DO 10 I = 1,10
         ID = APARM(I) + 0.5
         IS = BPARM(I) + 0.5
         IF ((ID.GT.0) .AND. (IS.GT.0)) THEN
            IF (I.NE.1) THEN
               CALL FINDIT (ID, IS, INLUN, IERR)
               IF (IERR.NE.0) GO TO 980
               END IF
            CALL ADDIT (I, INLUN, IOTLUN, IOTLU2, IERR)
            IF (IERR.NE.0) GO TO 980
            IDG = ID
            ISG = IS
            END IF
 10      CONTINUE
C                                       transpose
      CALL FIXIT (IOTLUN, IOTLU2, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Create, copy, update hist file.
      CALL FINDIT (IDG, ISG, INLUN, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL HISTRY (INLUN, IOTLUN, IHINL, IHOTL)
C                                       Release AIPS if necessary,
C                                       close, clear files, delete any
C                                       new files if error.
 980  CALL TSKEND (IERR)
C
 999  STOP
      END
      SUBROUTINE SETUP (PRGNAM, INLUN, IOTLUN, IOTLU2, IERR)
C-----------------------------------------------------------------------
C   SETUP will initialize the WAWA IO package, open the 1st input image,
C   create the output images, determine the name strings of the output
C   image and initialize the output image.
C   Inputs:
C      PRGNAM   C*6   Program name.
C      INLUN    I     Input image logical unit number.
C      IOTLUN   I     New image input logical unit number.
C      IOTLU2   I     Output LUN for new image.
C   Outputs:
C      IERR     I     Standard WAWA IO error message.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   INLUN, IOTLUN, IOTLU2, IERR
C
      CHARACTER AXTYP*8
      REAL      ZBLC(7), ZTRC(7), USER
      HOLLERITH MAP, SCLS(2)
      INTEGER   NOPARM, ID, IS, I, J, K
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'LAYER.INC'
C-----------------------------------------------------------------------
      NOPARM = 103
      CALL RFILL (7, 0.0, ZBLC)
      CALL RFILL (7, 0.0, ZTRC)
      CALL TSKBEG (PRGNAM, NOPARM, XNAM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fix the output image name string
C
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL CHR2H (6, 'SLAYER', 1, SCLS)
      USER = NLUSER
      CALL H2WAWA (XNAM, XCLS, XSEQ, MAP, XDISK, USER, OUFILE)
      CALL H2WAWA (XNAM, SCLS, 0.0, MAP, 0.0, USER, OUFIL2)
C                                       Open the old file. Get header.
      ID = APARM(1) + 0.5
      IS = BPARM(1) + 0.5
      IF ((ID.LE.0) .OR. (IS.LE.0)) THEN
         IERR = 8
         MSGTXT = 'AT LEAST ONE DISK AND CATALOG NUMBER MUST BE GIVEN'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL FINDIT (ID, IS, INLUN, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Fix BLC, TRC.
      CALL COPY (256, OLDI, NEWI)
      CALL HDRWIN (BLC, TRC, NEWI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL COPY (256, NEWI, WRKI)
C                                       combine RGB axis w 1st axis for
C                                       now
      WRKI(KINAX) = WRKI(KINAX) * 3
C                                       where is new axis?
      NROW = 1
      AX = 0
      J = NEWI(KIDIM)
      DO 10 I = 2,J
         IF (NEWI(KINAX+I-1).GT.1) THEN
            NROW = NROW * NEWI(KINAX+I-1)
         ELSE
            AX = I
            GO TO 20
            END IF
 10      CONTINUE
      AX = J + 1
 20   IF (AX.GT.7) THEN
         MSGTXT = 'TOO MANY REAL AXES - CANNOT ADD ANOTHER'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
C                                       move axes AX - J by 1.
      K = J
      DO 30 I = AX,J
         NEWI(KINAX+K) = NEWI(KINAX+K-1)
         NEWR(KRCRP+K) = NEWR(KRCRP+K-1)
         NEWR(KRCRT+K) = NEWR(KRCRT+K-1)
         NEWR(KRCIC+K) = NEWR(KRCIC+K-1)
         NEWD(KDCRV+K) = NEWD(KDCRV+K-1)
         NEWH(KHCTP+2*K) = NEWH(KHCTP+2*K-2)
         NEWH(KHCTP+2*K+1) = NEWH(KHCTP+2*K-1)
         K = K - 1
 30      CONTINUE
C                                       insert RGB axis
      NEWI(KIDIM) = NEWI(KIDIM) + 1
      NEWI(KINAX+K) = 3
      NEWR(KRCRP+K) = 1.0
      NEWR(KRCRT+K) = 0.0
      NEWR(KRCIC+K) = 1.0
      NEWD(KDCRV+K) = 1.0D0
      AXTYP = 'RGB'
      CALL CHR2H (8, AXTYP, 1, NEWH(KHCTP+2+K))
      NEWR(KRBLK) = FBLANK
      WRKR(KRBLK) = FBLANK
C                                       create output image
      CALL MAPCR (INFILE, OUFILE, NEWI, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       create work file
      CALL MAPCR (INFILE, OUFIL2, WRKI, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       open work for read & write
      CALL OPENCF (IOTLUN, OUFIL2, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL OPENCF (IOTLU2, OUFIL2, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Get new improved header.
      CALL GETHDR (IOTLU2, WRKI, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Init output image to blanks
      CALL MAPWIN (IOTLUN, ZBLC, ZTRC, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL MFILL (IOTLUN, FBLANK, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 970
      DO 40 I = 1,10
         IF (EPARM(I).LE.0.0) EPARM(I) = 1.0
 40      CONTINUE
      GO TO 999
C                                       error handling.
 970  CALL PRTERR (IERR, IOTLU2)
      GO TO 999
 990  CALL PRTERR (IERR, 0)
C
 999  RETURN
      END
      SUBROUTINE FINDIT (ID, IS, LUN, IERR)
C-----------------------------------------------------------------------
C   FINDIT finds the image at the specified disk and catalog number and
C   opens it
C   Inputs:
C      ID    I   Disk number
C      IS    I   Catalog number
C      LUN   I   LUN to use
C   Outputs:
C      IERR   I   Error code
C-----------------------------------------------------------------------
      INTEGER   ID, IS, LUN, IERR
C
      CHARACTER NAM*12, CLS*6, PTYPE*2, STAT*4
      INTEGER   SEQ, USID, BUFF(256)
      REAL      USER, ASEQ, ADISK
      HOLLERITH MAP, ANAM(3), ACLS(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'LAYER.INC'
C-----------------------------------------------------------------------
C                                       use CATDIR;s INFO routine
      CALL CATDIR ('INFO', ID, IS, NAM, CLS, SEQ, PTYPE, USID, STAT,
     *   BUFF, IERR)
      IF (IERR.EQ.6) THEN
         WRITE (MSGTXT,1000) ID, IS
         GO TO 900
      ELSE IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1005) ID, IS, IERR
         GO TO 900
         END IF
C                                       build file name
      CALL CHR2H (12, NAM, 1, ANAM)
      CALL CHR2H (6, CLS, 1, ACLS)
      ASEQ = SEQ
      ADISK = ID
      CALL CHR2H (4, 'MA  ', 1, MAP)
      USER = NLUSER
      CALL H2WAWA (ANAM, ACLS, ASEQ, MAP, ADISK, USER, INFILE)
C                                       open and get header
      CALL OPENCF (LUN, INFILE, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETHDR (LUN, OLDI, IERR)
      GO TO 999
C
 900  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MAP ON DISK',I3,' CNO,',I6,' NOT FOUND')
 1005 FORMAT ('MAP ON DISK',I3,' CNO,',I6,' CATDIR IERR',I4)
      END
      SUBROUTINE ADDIT (IMG, INLUN, IOTLUN, IOTLU2, IERR)
C-----------------------------------------------------------------------
C   ADDIT will add the current input image to the current sum, doing the
C   appropriate absorption and emission
C   Inputs:
C      INLUN    I   The logical unit number of the old image.
C      IOTLUN   I   The input logical unit no. of the new image.
C      IOTLU2   I   The output logical unit no. of the new image.
C   Outputs:
C      IERR     I   error code. 0 = ok.
C-----------------------------------------------------------------------
      INTEGER   IMG, INLUN, IOTLUN, IOTLU2, IERR
C
      REAL      ZBLC(7), ZTRC(7), TB, RMIN, RMAX, TAU, TAUE(3), TAUA(3),
     *   D0
      INTEGER   NX, IX, I, IROW, J, INSLT2, OUTSLT, OUTVOL, INVOL2,
     *   SCRTCH(256), IPTR
      LOGICAL   DOCLR
      CHARACTER DCH*12
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'LAYER.INC'
C-----------------------------------------------------------------------
C                                       Initialize ZBLC & ZTRC to zero.
      CALL RFILL (7, 0.0, ZBLC)
      CALL RFILL (7, 0.0, ZTRC)
      NX = NEWI(KINAX)
      RMIN = OLDR(KRDMN)
      RMAX = OLDR(KRDMX)
      IF (RMAX.LE.RMIN) THEN
         IERR = 8
         WRITE (MSGTXT,1000) IMG, RMIN, RMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       prepare I/O
      CALL MAPWIN (INLUN, BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL MAPWIN (IOTLUN, ZBLC, ZTRC, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL MAPWIN (IOTLU2, ZBLC, ZTRC, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Figure out if IN/OUT file.
      DO 10 IPTR = 1,EFIL
         IF (FILTAB(POLUN,IPTR).EQ.IOTLUN) THEN
            INSLT2 = FILTAB(POCAT,IPTR)
            INVOL2 = FILTAB(POVOL,IPTR)
            END IF
         IF (FILTAB(POLUN,IPTR).EQ.IOTLU2) THEN
            OUTSLT = FILTAB(POCAT,IPTR)
            OUTVOL = FILTAB(POVOL,IPTR)
            END IF
 10      CONTINUE
C                                       If in equals out we must clear
C                                       READ later on.
      DOCLR = (OUTVOL.EQ.INVOL2) .AND. (OUTSLT.EQ.INSLT2)
C                                       read row loop
      TB = 1.0
      DO 100 IROW = 1,NROW
         CALL MAPIO ('READ', INLUN, BUFF1, IERR)
         IF (IERR.NE.0) GO TO 970
C
         IF (DOCLR) CALL CATDIR ('CSTA', INVOL2, INSLT2, DCH, DCH, D0,
     *      DCH, D0, 'CLWR', SCRTCH, IERR)
         CALL MAPIO ('READ', IOTLU2, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       If IN2 = OUT we must clear this
C                                       READ so we can set WRIT status.
         IF (DOCLR) THEN
            CALL CATDIR ('CSTA', INVOL2, INSLT2, DCH, DCH, D0, DCH, D0,
     *         'CLRD', SCRTCH, IERR)
            DOCLR = .FALSE.
            END IF
C                                       loop through row
         DO 90 IX = 1,NX
            IF (BUFF1(IX).NE.FBLANK) THEN
               TAU = (BUFF1(IX) - RMIN) / (RMAX - RMIN)
               IF (DOWT.GT.0.0) TB = TAU
               TAUE(1) = DPARM(IMG) * PLCOLR(1,IMG) * TAU
               TAUE(2) = DPARM(IMG) * PLCOLR(2,IMG) * TAU
               TAUE(3) = DPARM(IMG) * PLCOLR(3,IMG) * TAU
               IF (DOCOLR.GT.0.0) THEN
                  TAUA(1) = CPARM(IMG) * PLCOLR(1,IMG) * TAU
                  TAUA(2) = CPARM(IMG) * PLCOLR(2,IMG) * TAU
                  TAUA(3) = CPARM(IMG) * PLCOLR(3,IMG) * TAU
               ELSE
                  TAUA(1) = CPARM(IMG) * TAU
                  TAUA(2) = CPARM(IMG) * TAU
                  TAUA(3) = CPARM(IMG) * TAU
                  END IF
               J = 3 * (IX - 1)
               DO 20 I = 1,3
                  J = J + 1
                  IF (BUFF2(J).NE.FBLANK) THEN
                     BUFF2(J) = BUFF2(J) * EXP (-TAUA(I))
                  ELSE
                     BUFF2(J) = 0.0
                     END IF
                  BUFF2(J) = BUFF2(J) + EPARM(IMG) * TB *
     *               (1.0 - EXP (-TAUE(I)))
 20               CONTINUE
               END IF
 90         CONTINUE
         CALL MAPIO ('WRIT', IOTLUN, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
      CALL FILCLS (INLUN)
      GO TO 999
C                                       errors
 970  CALL PRTERR (IERR, INLUN)
      GO TO 999
 980  CALL PRTERR (IERR, IOTLUN)
      GO TO 999
 990  CALL PRTERR (IERR, IOTLU2)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMAGE',I3,' MIN/MAX=',2(1PE12.3),' NOT USABLE')
      END
      SUBROUTINE FIXIT (IOTLUN, IOTLU2, IERR)
C-----------------------------------------------------------------------
C   FIXIT determines the max/min of the resulting data and transposes
C   the RGB triplets into 3 output planes
C   Inputs:
C      IOTLUN   I   LUN of output image (starts open to scratch)
C      IOTLU2   I   LUN of scratch image
C   Output:
C      IERR     I   Error code
C-----------------------------------------------------------------------
      INTEGER   IOTLUN, IOTLU2, IERR
C
      REAL      RMAX, RMIN, ZBLC(7), ZTRC(7)
      INTEGER   IROW, I, J, IC, NX
      LOGICAL   BLNKD
      INCLUDE 'LAYER.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      CALL RFILL (7, 0.0, ZBLC)
      CALL RFILL (7, 0.0, ZTRC)
      NX = NEWI(KINAX)
      BLNKD = .FALSE.
C                                       close 2nd scratch LUN
      CALL FILCLS (IOTLUN)
C                                       open output file
      CALL OPENCF (IOTLUN, OUFILE, IERR)
      IF (IERR.NE.0) GO TO 980
      RMAX = -1.E20
      RMIN = 1.E20
C                                       init full output
      CALL MAPWIN (IOTLUN, ZBLC, ZTRC, IERR)
      IF (IERR.NE.0) GO TO 980
      DO 100 IC = 1,3
C                                       reinit each color
         CALL MAPWIN (IOTLU2, ZBLC, ZTRC, IERR)
         IF (IERR.NE.0) GO TO 990
         DO 90 IROW = 1,NROW
            CALL MAPIO ('READ', IOTLU2, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 990
            J = IC
            DO 20 I = 1,NX
               IF (BUFF1(J).EQ.FBLANK) THEN
                  BUFF2(I) = FBLANK
                  BLNKD = .TRUE.
               ELSE
                  BUFF2(I) = BUFF1(J)
                  RMAX = MAX (RMAX, BUFF2(I))
                  RMIN = MIN (RMIN, BUFF2(I))
                  END IF
               J = J + 3
 20            CONTINUE
            CALL MAPIO ('WRIT', IOTLUN, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 980
 90         CONTINUE
 100     CONTINUE
      NEWR(KRDMX) = RMAX
      NEWR(KRDMN) = RMIN
      IF (BLNKD) THEN
         NEWR(KRBLK) = FBLANK
      ELSE
         NEWR(KRBLK) = 0.0
         END IF
C                                       close and delete scratch
      CALL FILCLS (IOTLU2)
      CALL FILDES (OUFIL2, .FALSE., '  ', 0, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'ERROR DELETING SCRATCH IMAGE'
         CALL MSGWRT (6)
         IERR = 0
         END IF
      GO TO 999
C
 980  CALL PRTERR (IERR, IOTLUN)
      GO TO 999
 990  CALL PRTERR (IERR, IOTLU2)
C
 999  RETURN
      END
      SUBROUTINE HISTRY (INLUN, IOTLUN, IHINL, IHOTL)
C-----------------------------------------------------------------------
C   HISTRY will create and write the history file for the new image.
C   Inputs:
C      INLUN    I        the LUN for the input map.
C      IOTLUN   I        the LUN for the output map.
C      IHINL    I        the LUN to use for the input history file.
C      IHOTL    I        the LUN to use for the new output history file.
C-----------------------------------------------------------------------
      INTEGER   INLUN, IOTLUN, IHINL, IHOTL
C
      CHARACTER NAMSTR*36, HILINE*72, NAME*12, CLASS*6, PTYPE*2
      INTEGER   SEQ, VOL, USID, IBLC(7), ITRC(7), INPTR, IOPTR, IPTR,
     *   IERR, I, IERR2, ID, IS
      LOGICAL   UPDATE
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'LAYER.INC'
      DATA UPDATE /.TRUE./
C-----------------------------------------------------------------------
C                                       Get additional data from commo
      INPTR = 0
      IOPTR = 0
      DO 10 IPTR = 1,EFIL
         IF (INLUN.EQ.FILTAB(POLUN,IPTR)) INPTR = IPTR
         IF (IOTLUN.EQ.FILTAB(POLUN,IPTR)) IOPTR = IPTR
 10      CONTINUE
C                                       Check for file not open error.
      IF ((INPTR.EQ.0) .OR. (IOPTR.EQ.0)) THEN
         MSGTXT = 'HISTRY: IN OR OUT FILE NOT OPEN'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       copy keywords
      CALL KEYPCP (FILTAB(POVOL,INPTR), FILTAB(POCAT,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,IOPTR), 0, ' ', IERR)
C                                       Create history file and
C                                       copy HI of INSEQ
      CALL HIINIT (2)
      CALL HISCOP (IHINL, IHOTL, FILTAB(POVOL,INPTR),
     *   FILTAB(POVOL,IOPTR), FILTAB(POCAT,INPTR), FILTAB(POCAT,IOPTR),
     *   NEWI, BUFF2, BUFF1, IERR)
      IF (IERR.GT.2) GO TO 100
C                                       Add LAYER history.
C                                       Input map.
      CALL GTNAME (INLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCO1 (TSKNAM, NAME, CLASS, SEQ, VOL, IHOTL, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Output map.
      CALL GTNAME (IOTLUN, NAMSTR, IERR)
      IF (IERR.NE.0) GO TO 100
      CALL WAWA2A (NAMSTR, NAME, CLASS, SEQ, PTYPE, VOL, USID)
      CALL HENCOO (TSKNAM, NAME, CLASS, SEQ, VOL, IHOTL, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       BLC, TRC
      DO 20 I = 1,7
         IBLC(I) = BLC(I) + .5
         ITRC(I) = TRC(I) + .5
 20      CONTINUE
      WRITE (HILINE,1020) TSKNAM, 'BLC', IBLC
      CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1020) TSKNAM, 'TRC', ITRC
      CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       image parameters
      DO 30 I = 1,10
         ID = APARM(I) + 0.5
         IS = BPARM(I) + 0.5
         IF ((ID.GT.0) .AND. (IS.GT.0)) THEN
            WRITE (HILINE,1030) TSKNAM, I, ID, IS
            CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (HILINE,1031) TSKNAM, I, CPARM(I)
            CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (HILINE,1032) TSKNAM, I, DPARM(I)
            CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (HILINE,1033) TSKNAM, I, EPARM(I)
            CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 100
            WRITE (HILINE,1034) TSKNAM, I, PLCOLR(1,I), PLCOLR(2,I),
     *         PLCOLR(3,I)
            CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
 30      CONTINUE
      IF (DOCOLR.GT.0.0) THEN
         HILINE = TSKNAM // 'DOCOLOR = 1  / absorption is colored'
      ELSE
         HILINE = TSKNAM // 'DOCOLOR = -1  / absorption is not colored'
         END IF
      CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (DOWT.GT.0.0) THEN
         HILINE = TSKNAM // 'DOWEIGHT = 1  / emission scaled' //
     *      ' by brightness'
      ELSE
         HILINE = TSKNAM // 'DOWEIGHT = -1  / emission not scaled' //
     *      ' by brightness'
         END IF
      CALL HIADD (IHOTL, HILINE, BUFF1, IERR)
C                                       Close HI file
 100  CALL HICLOS (IHOTL, UPDATE, BUFF1, IERR2)
C                                       Report any IERRs
      IF ((IERR.NE.0) .OR. (IERR2.NE.0)) THEN
         MSGTXT = 'WARNING. ERROR WRITING HISTORY FILE'
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A6,A3,' =',7I6)
 1030 FORMAT (A6,'/IMAGE',I2,' DISK',I3,' CNO',I6)
 1031 FORMAT (A6,'CPARM(',I2,') =',F7.3,'  / absorption factor')
 1032 FORMAT (A6,'DPARM(',I2,') =',F7.3,'  / emission factor')
 1033 FORMAT (A6,'EPARM(',I2,') =',F7.3,'  / scale emitor')
 1034 FORMAT (A6,'PLCOLORS(*,',I2,') =',3F6.3,'  / RGB color')
      END

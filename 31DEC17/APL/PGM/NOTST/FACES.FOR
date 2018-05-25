LOCAL INCLUDE 'FACES.INC'
      HOLLERITH XNAMEI(3), XCLASI(2), XNAMO(3), XXSOUR(4,30),
     *   XINFIL(12)
      REAL       DISKIN, XSEQ, XDISKO, CELSIZ(2), XSIZE(2), SHIFT(2),
     *   BMAJ, BFLUX, BPARM(10), PBPARM(7)
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQ, DISKIN, XXSOUR, XNAMO,
     *   XDISKO, CELSIZ, XSIZE, SHIFT, BMAJ, BFLUX, BPARM, PBPARM,
     *   XINFIL
C
      DOUBLE PRECISION RA0, DEC0
      INTEGER   IMSIZE(2), OVRLAP, CATNO, SEQIN, IDISK, LUNNV, INDNV,
     *   ODISK, OSEQ, OCNO, OFIELD, SCRTCH(512)
      CHARACTER NAMEIN*12, CLASIN*6, NAMOUT*12, NVSSF*48, MSOURC*16,
     *   CLSOUT*6
      COMMON /FACESP/ RA0, DEC0, SCRTCH, IMSIZE, OVRLAP, CATNO, SEQIN,
     *   IDISK, LUNNV, INDNV, ODISK, OSEQ, OCNO, OFIELD
      COMMON /FACESC/ NAMEIN, CLASIN, CLSOUT, NVSSF, MSOURC, NAMOUT
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
LOCAL END
      PROGRAM FACES
C-----------------------------------------------------------------------
C! Make images/CCs from NVSS
C# Imaging Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2000-2003, 2005, 2009, 2011-2012, 2015
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
C   FACES computes fields like SETFC, but then makes images of the
C   catalog sources that fall within the central fly's eye.  These
C   images with their CC files may be an initial model for calibration.
C   Fields with no sources are not written or counted.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME       XNAMEI/NAMEIN    Name of input UV data.
C      INCLASS      XCLASI/CLASIN    Class of input UV data.
C      INSEQ        SEQ/SEQIN        Seq. of input UV data.
C      INDISK       DISKIN/IDISK     Disk number of input UV data.
C      SOURCES      XXSOUR/SOURCE    Source which will be mapped.
C      OUTNAME      XNAMO/NAMOUT     Output file name
C      OUTDISK      ODISK            Output file disk number
C      CELLSIZE     CELSIZ           pixel size in image.
C      IMSIZE       XSIZE/IMSIZE     image size, also field size.
C      SHIFT        SHIFT            offset for all fields.
C      BPARM        BPARM            1. Fly-eye radius (deg)
C                                    2. Overlap in fly-eye
C      PBPARM       PBPARM           Primary beam cutoff, flag, parms
C      INFILE       XINFIL/NVSSF     NVSS input file name.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXF
      PARAMETER (MAXF=20*MAXFLD)
C
      CHARACTER PNAM*6, MTYPE*2, STAT*4, EXT*4, TYPE*2, PHNAME*48,
     *   ZTXO*4, ANVSSF*48
      INTEGER   NPARMS, IRET, IERR, IROUND, LUNSU, SQUAL, SOUID, NID,
     *   FREQAX, II, MLUN(2), MIND(2), CTTUV(256), JJ, I, NCC, IVER
      REAL      RADIUS, CTTUVR(256), AFLUX, CATR(256)
      DOUBLE PRECISION LAMBD, RAS(MAXF), DECS(MAXF), CTTUVD(128),
     *   CATD(128), DTEMP
      HOLLERITH CTTUVH(256), CATH(256)
      LOGICAL   NVOPEN, NEW, STD, MULTI
      INCLUDE 'FACES.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CTTUV, CTTUVR, CTTUVH, CTTUVD)
      INCLUDE 'INCS:PSTD.INC'
      DATA NPARMS, PNAM /168, 'FACES '/
      DATA MLUN, LUNSU /16,17, 27/
      DATA NID, SQUAL /1, -1/
      DATA MTYPE /'UV'/
C-----------------------------------------------------------------------
C                                       initialize
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NCFILE = 0
      ZTXO = 'READ'
C                                       get the input parameters
      CALL GTPARM (PNAM, NPARMS, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IRET = 1
         RQUICK = .FALSE.
         IF (IERR.EQ.1) THEN
            MSGTXT = 'CANNOT FIND INITIATOR IN GTPARM'
            CALL MSGWRT (1)
            GO TO 999
         ELSE
            MSGTXT= 'DISK PROBLEMS IN GTPARM'
            CALL MSGWRT (6)
            GO TO 995
            END IF
         END IF
      IRET = 0
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
C                                       set parameters
      LUNNV = 11
      NVOPEN = .FALSE.
      IMSIZE(1) = IROUND (XSIZE(1))
      IMSIZE(2) = IROUND (XSIZE(2))
      OVRLAP = IROUND (BPARM(2))
      IF (OVRLAP.LT.2) OVRLAP = 5
      BPARM(2) = OVRLAP
      IF ((PBPARM(1).LE.0.0) .OR. (PBPARM(1).GT.0.9)) PBPARM(1) = 0.023
      IF (BPARM(3).LE.0.0) BPARM(3) = 1.0
      CALL H2CHR (12, 1, XNAMO, NAMOUT)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (48, 1, XINFIL, NVSSF)
      SEQIN = IROUND (XSEQ)
      IDISK = IROUND (DISKIN)
      ODISK = IROUND (XDISKO)
      IF ((BMAJ.GT.0.0) .AND. (CELSIZ(1).NE.0.0)) THEN
         BMAJ = BMAJ / ABS (CELSIZ(1))
      ELSE
         BMAJ = 3.0
         END IF
      OSEQ = 0
C                                       locate map in directory
      CATNO = 1
      CALL CATDIR ('SRCH', IDISK, CATNO, NAMEIN, CLASIN, SEQIN,
     *   MTYPE, NLUSER, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'REQUESTED DATA NOT FOUND IN CATALOG DIRECTORY'
         CALL MSGWRT (6)
         GO TO 995
         END IF
C                                       read catalog block
      CALL CATIO ('READ', IDISK, CATNO, CATBLK, 'REST', SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (6)
         GO TO 995
         END IF
      WRITE (MSGTXT,1010) NAMEIN, CLASIN, SEQIN, IDISK, CATNO
      CALL MSGWRT (3)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 995
      CALL COPY (256, CATBLK, CTTUV)
      UNAME = NAMEIN
      UCLAS = CLASIN
      UDISK = IDISK
      USEQ = SEQIN
C                                       get antenna info
      IVER = 1
      CALL ANTINI ('READ', ANBUFF, IDISK, CATNO, IVER, CATBLK, LUNSU,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANFQID, ANTNIF, IERR)
      CALL TABIO ('CLOS', 0, 1, ANBUFF, ANBUFF, IERR)
C                                       single-source use UVPGET output
      CALL MULSDB (CATBLK, MULTI)
      IF (.NOT.MULTI) THEN
         SOUID = -1
         RA0 = RA
         DEC0 = DEC
         IF ((RA0.EQ.0.0D0) .AND. (DEC0.EQ.0.0D0)) THEN
            MSGTXT = 'CANNOT FIND SOURCE POSITION'
            GO TO 980
            END IF
C                                       multi-source get 1 source
      ELSE
         CALL H2CHR (16, 1, XXSOUR(1,1), MSOURC)
C                                       multi-source file, but no source
C                                       specified...
         IF (MSOURC.EQ.' ') THEN
            MSGTXT = 'MULTI-SOURCE FILE BUT NO SOURCE SPECIFIED!'
            GO TO 980
C                                       find source...
         ELSE
            CALL SOURNU (MSOURC, SQUAL, 1, IDISK, CATNO, NID, SCRTCH,
     *         SOUID, IRET)
            IF (IRET.EQ.0) THEN
               CALL GETSOU (SOUID, IDISK, CATNO, CATBLK, LUNSU, IRET)
               IF (IRET.EQ.0) THEN
                  DEC0 = DECEPO * RAD2DG
                  RA0 = RAEPO * RAD2DG
               ELSE
                  MSGTXT = 'ERROR ACCESSING SU EXTENSION'
                  GO TO 980
                  END IF
            ELSE
               MSGTXT = 'ERROR FINDING REQUESTED SOURCE'
               GO TO 980
               END IF
            END IF
         END IF
C                                       Get CELSIZ, IMSIZE
      RADIUS = BPARM(1)
      IF ((CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0) .OR.
     *   (IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0)) THEN
         CALL SETCEL (RADIUS, IMSIZE, CELSIZ, IRET)
         IF (IRET.NE.0) GO TO 995
         END IF
C                                       Compute fly's eye
      CALL FLYEYE (RADIUS, IMSIZE, CELSIZ, OVRLAP, SHIFT, RA0, DEC0,
     *   OFIELD, RAS, DECS, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Catalog file name
      IF (NVSSF.EQ.' ') THEN
         EXT = 'NVSS'
         IF (ABS(CATR(KREPO)-1950.0).LT.0.1) THEN
            NVSSF = 'AIPSTARS:NV50.0030'
         ELSE
            NVSSF = 'AIPSTARS:NV00.0030'
            END IF
         END IF
      ANVSSF = NVSSF
      STD = NVSSF(:8).EQ.'AIPSTARS'
      IF (DEC0+RADIUS.GT.90.0D0) STD = .FALSE.
      IF (DEC0-RADIUS.LT.-90.0D0) STD = .FALSE.
      IF (STD) THEN
         DTEMP = RADIUS / COS (DG2RAD * DEC0)
         IF (RA0-DTEMP.LT.0.0D0) STD = .FALSE.
         IF (RA0+DTEMP.GT.360.0D0) STD = .FALSE.
         END IF
C                                       make a subset file
      IF (OFIELD.GT.3) THEN
         AFLUX = BFLUX / BPARM(3)
         CALL SUBFLD (STD, AFLUX, RADIUS, RA0, DEC0, NVSSF, ANVSSF,
     *      IRET)
         IF (IRET.NE.0) GO TO 990
         ZTXO = 'QRED'
         STD = .FALSE.
         END IF
C                                       find frequency
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), FREQAX,
     *   IRET)
      IF (IRET.EQ.0) THEN
         FREQ = CATD(KDCRV+FREQAX)
         LAMBD = VELITE / FREQ
      ELSE
         LAMBD = 0.0D0
         END IF
C                                       Prepare output file header
      CALL CHR2H (8, 'JY/BEAM ', 1, CATH(KHBUN))
      CALL CHR2H (8, 'RA---SIN', 1, CATH(KHCTP))
      CALL CHR2H (8, 'DEC--SIN', 1, CATH(KHCTP+2))
      CALL CHR2H (8, 'FREQ    ', 1, CATH(KHCTP+4))
      CALL CHR2H (8, 'STOKES  ', 1, CATH(KHCTP+6))
      CALL CHR2H (8, '        ', 1, CATH(KHCTP+8))
      CALL CHR2H (8, '        ', 1, CATH(KHCTP+10))
      CALL CHR2H (8, '        ', 1, CATH(KHCTP+12))
      CATBLK(KIGCN) = 0
      CATBLK(KIPCN) = 0
      CATBLK(KIDIM) = 4
      CATBLK(KINAX) = IMSIZE(1)
      CATBLK(KINAX+1) = IMSIZE(2)
      CATR(KRCRP) = IMSIZE(1)/2
      CATR(KRCRP+1) = IMSIZE(2)/2 + 1
      CATR(KRCRP+2) = 1.0
      CATR(KRCRP+3) = 1.0
      CALL FILL (5, 1, CATBLK(KINAX+2))
      CALL RFILL (7, 0.0, CATR(KRCRT))
      CALL RFILL (3, 1.0, CATR(KRCRP+4))
      CALL RFILL (3, 0.0, CATR(KRCRP+4))
      CATR(KRCIC) = -ABS(CELSIZ(1)) / 3600.0
      CATR(KRCIC+1) = ABS(CELSIZ(2)) / 3600.0
      CATR(KRCIC+2) = CTTUVR(KRCIC+JLOCF)
      CATR(KRCIC+3) = 1.0
      CATD(KDCRV+2) = CTTUVD(KDCRV+JLOCF)
      CATD(KDCRV+3) = 1.0D0
      CATD(KDCRV+4) = 0.0D0
      CATD(KDCRV+5) = 0.0D0
      CATD(KDCRV+6) = 0.0D0
      CATR(KRBMJ) = BMAJ * CATR(KRCIC+1)
      CATR(KRBMN) = CATR(KRBMJ)
      IF (NAMOUT.EQ.' ') NAMOUT = NAMEIN
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      TYPE = 'MA'
      CALL CHR2H (2, TYPE, KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = OSEQ
      CATBLK(KIIMU) = NLUSER
C                                       loop over output fields
      JJ = 1
      NEW = .TRUE.
      DO 100 II = 1,OFIELD
         CATD(KDCRV) = RAS(II)
         CATD(KDCRV+1) = DECS(II)
         CATD(KDORA) = RA0
         CATD(KDODE) = DEC0
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
C                                       overwrite empty ones
C                                       create when last had something
         IF (NEW) THEN
            IF (JJ.GT.MAXFLD) THEN
               MSGTXT = 'WARNING: TOO MANY FIELDS'
               CALL MSGWRT (7)
               IF (JJ.GT.999) THEN
                  IRET = 10
                  GO TO 999
                  END IF
               END IF
            WRITE (CLSOUT,1020) JJ
            CALL CHR2H (6, CLSOUT, KHIMCO, CATH(KHIMC))
C                                       Create output map file.
            CALL MCREAT (ODISK, OCNO, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1025) IRET, JJ
               GO TO 990
               END IF
            OSEQ = CATBLK(KIIMS)
            NEW = .FALSE.
C                                       Mark in /CFILE/
            NCFILE = NCFILE + 1
            FCNO(NCFILE) = OCNO
            FVOL(NCFILE) = ODISK
            FRW(NCFILE) = 2
C                                       open image file
            CALL ZPHFIL ('MA', ODISK, OCNO, 1, PHNAME, IERR)
            CALL ZOPEN (MLUN(2), MIND(2), ODISK, PHNAME, .TRUE.,
     *         .FALSE., .TRUE., IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1030) II, IRET
               CALL MSGWRT (8)
               GO TO 990
               END IF
            CALL MAPOPN ('INIT', ODISK, NAMOUT, CLSOUT, OSEQ, TYPE,
     *         NLUSER, MLUN, MIND, OCNO, CATBLK, SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1031) II, IRET
               CALL MSGWRT (8)
               GO TO 990
               END IF
            END IF
C                                       open catalog
         CALL ZTXOPN (ZTXO, LUNNV, INDNV, ANVSSF, .FALSE., IRET)
         ZTXO = 'QRED'
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1040) IRET
            GO TO 980
            END IF
         NVOPEN = .TRUE.
C                                       catalog sources to image
         CALL ADNVSS (STD, MLUN, MIND, II, RAS, DECS, JJ, NCC, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1050) II, IRET
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       close catalog
         CALL ZTXCLS (LUNNV, INDNV, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1060) IERR
            CALL MSGWRT (6)
            END IF
         NVOPEN = .FALSE.
C                                       Keep this one
         IF (IRET.EQ.0) THEN
            CATBLK(KINIT) = NCC
            CATBLK(KITYP) = 2
C                                       History
            CALL FACEHI
C                                       close image
            CALL MAPCLS ('WRIT', ODISK, OCNO, MLUN, MIND, CATBLK,
     *         .TRUE., SCRTCH, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1070) II, IRET
               GO TO 980
               END IF
            NCFILE = NCFILE - 1
            CALL ZCLOSE (MLUN(2), MIND(2), IRET)
            JJ = JJ + 1
            NEW = .TRUE.
C                                       ignore this one
         ELSE
C                                       must destroy too
            IF (II.EQ.OFIELD) THEN
C                                       close image
               CALL MAPCLS ('WRIT', ODISK, OCNO, MLUN, MIND, CATBLK,
     *            .TRUE., SCRTCH, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1070) II, IERR
                  CALL MSGWRT (8)
                  END IF
               NCFILE = NCFILE - 1
               CALL ZCLOSE (MLUN(2), MIND(2), IRET)
               CALL MDESTR (ODISK, OCNO, CATBLK, SCRTCH, I, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1090) IERR, OCNO
                  CALL MSGWRT (8)
                  END IF
               END IF
            END IF
 100     CONTINUE
      GO TO 990
C                                       Error
 980  CALL MSGWRT (8)
C                                       close files and go to bed.
 990  IF (NVOPEN) CALL ZTXCLS (LUNNV, INDNV, IERR)
      IF (ANVSSF.NE.NVSSF) CALL ZTXZAP (LUNNV, ANVSSF, IERR)
C                                       close files and go to bed.
 995  CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('CATIO ERROR NO', I5)
 1010 FORMAT ('Found ', A12, A6, ' Seq', I4, ' Disk:', I3,' in slot',
     *   I5)
 1020 FORMAT ('IMO',I3.3)
 1025 FORMAT ('ERROR',I5,' CREATING IMAGE FOR OUTPUT FIELD',I4)
 1030 FORMAT ('FIELD',I6,' ERROR',I5,' OPENING IMAGE FILE')
 1031 FORMAT ('FIELD',I6,' ERROR',I5,' OPENING IMAGE FILE A 2ND TIME')
 1040 FORMAT ('ERROR NO', I5, ' OPENING NVSS FILE')
 1050 FORMAT ('FIELD',I6,' ERROR',I5,' ADDING NVSS TO IMAGE FILE')
 1060 FORMAT ('ERROR NO', I5, ' CLOSING NVSS FILE')
 1070 FORMAT ('FIELD',I6,' ERROR',I5,' CLOSING IMAGE FILE')
 1090 FORMAT ('ERROR',I5,' DESTROYING THE FILE IN SLOT',I6)
      END
      SUBROUTINE SUBFLD (STD, AFLUX, RADIUS, RA0, DEC0, NVSSF, ANVSSF,
     *   IRET)
C-----------------------------------------------------------------------
C   Copies the portion of the NVSS file which may be useful to a scratch
C   file
C   Inputs:
C      STD      L      standard file
C      AFLUX    R      flux * beam limit / flux_rescale_factor
C      RADIUS   R      radius to include in degrees (2 X is kept)
C      RA0      D      center RA in deg
C      DEC0     D      center Declination in deg
C      NVSSF    C*48   input NVSS text file
C   Output:
C      ANVSSF   C*48   subset file to use
C      IRET     I      error code
C-----------------------------------------------------------------------
      LOGICAL   STD
      INTEGER   IRET
      REAL      AFLUX, RADIUS
      DOUBLE PRECISION RA0, DEC0
      CHARACTER NVSSF*(*), ANVSSF*(*)
C
      DOUBLE PRECISION SINDEC, COSDEC, SRA, XRA, XDEC, SINCAL, COSCAL,
     *   XD, XMIN, XMAX
      INTEGER   LUNNV, INDNV, LUNNO, INDNO, IS, OS, IFLUX, J, JTRIM,
     *   NMAX, NN, I, JT
      REAL      XF
      CHARACTER INLINE(25)*132
      LOGICAL   LSTD, DONE
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      DONE = .FALSE.
      LSTD = STD
      IF (STD) THEN
         XD = 2.5 * RADIUS / COSDEC
         XMIN = RA0 - XD
         XMAX = RA0 + XD
         LSTD = (XMIN.GT.0.0) .AND. (XMAX.LT.360.0)
         END IF
      IF (LSTD) THEN
         NMAX = 25
      ELSE
         NMAX = 1
         XMAX = 1.D4
         END IF
C                                       scratch file name
      ANVSSF = 'RUNFIL:NVSSHHHH.N.UUU'
      ANVSSF(12:15) = HSTNAM(:4)
      CALL ZEHEX (NPOPS, 1, ANVSSF(17:17))
      CALL ZEHEX (NLUSER, 3, ANVSSF(19:21))
C                                       open the files
      IS = 0
      OS = 0
      LUNNV = 11
      CALL ZTXOPN ('READ', LUNNV, INDNV, NVSSF, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'OPEN NVSS'
         GO TO 990
         END IF
      LUNNO = 10
      CALL ZTXOPN ('WRIT', LUNNO, INDNO, ANVSSF, .FALSE., IRET)
      IF (IRET.EQ.5) THEN
         MSGTXT = 'Temporary file already exists - deleting'
         CALL MSGWRT (6)
         CALL ZTXZAP (LUNNO, ANVSSF, IRET)
         IF (IRET.EQ.0) CALL ZTXOPN ('WRIT', LUNNO, INDNO, ANVSSF,
     *      .FALSE., IRET)
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'OPEN SUBSET FILE'
         GO TO 990
         END IF
C                                       loop in file
 10   NN = 1
 11   CALL ZTXIO ('READ', LUNNV, INDNV, INLINE(NN), IRET)
      IF (IRET.EQ.2) THEN
         NMAX = NN
         IRET = 0
         DONE = .TRUE.
         GO TO 15
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         JT = JTRIM (INLINE(NN))
         IF (INLINE(NN)(1:1).EQ.';') GO TO 11
         NN = NN + 1
         IF (NN.LE.NMAX) GO TO 11
         NN = NMAX
         END IF
C                                       finished a buffer
 15   IF (NMAX.GT.1) THEN
         READ (INLINE(NMAX),1010) XRA
         IF (XRA.LT.XMIN) GO TO 10
         NMAX = 1
         END IF
C                                       empty buffer
      DO 20 I = 1,NN
         READ (INLINE(I),1010) XRA, XDEC, IFLUX
         IF (XRA.GT.XMAX) GO TO 900
         XF = IFLUX / 1000.0
         IF (XF.GE.AFLUX) THEN
            XDEC = XDEC * DG2RAD
            XRA = XRA * DG2RAD
            SINCAL = SIN (XDEC)
            COSCAL = COS (XDEC)
            XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA - SRA)
            IF (XD.GT.1.0D0) THEN
               XD = 0.0D0
            ELSE
               XD = RAD2DG * ACOS (XD)
               END IF
            IS = IS + 1
            IF (XD.LE.2.0*RADIUS) THEN
               J = JTRIM (INLINE(I))
               CALL ZTXIO ('WRIT', LUNNO, INDNO, INLINE(I)(:J), IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1020) IRET, 'WRITE SUBSET'
                  GO TO 990
                  END IF
               OS = OS + 1
               END IF
            END IF
 20      CONTINUE
      IF (.NOT.DONE) GO TO 10
C
 900  CALL ZTXCLS (LUNNV, INDNV, IRET)
      CALL ZTXCLS (LUNNO, INDNO, IRET)
      WRITE (MSGTXT,1900) OS, IS
      CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SUBFLD: ERROR',I6,' ',A)
 1010 FORMAT (F9.5,1X,F9.5,I7)
 1900 FORMAT ('Copied',I8,' OF',I8,' sources to smaller file for speed')
      END
      SUBROUTINE FLYEYE (RADIUS, IMSIZE, PIXSIZ, OVRLAP, SHIFT, RA0,
     *   DEC0, OFIELD, RAS, DECS, IERR)
C-----------------------------------------------------------------------
C   Writes fly's eye portion of BOXFILE
C   Inputs:
C      IMSIZE   I(2)   Output image size
C      PIXSIZ   R(2)   Pixel size (asec)
C      OVRLAP   I      Number pixels of overlap
C      SHIFT    R(2)   Shift all coordinates
C      RA0      D      Center RA in degrees
C      DEC0     D      Center Dec in degrees
C      LUNBX    I      LUN of Box file
C      INDBX    I      FTAB pointer of box file
C      RADIUS   R      In: Radius of fly's eye in degree
C   Output:
C      OFIELD   I      Number fields in fly's eye
C      RAS      D(*)   RA (deg) if each field
C      DECS     D(*)   Dec (deg) of each field
C      IERR     I      Error code
C-----------------------------------------------------------------------
      DOUBLE PRECISION RA0, DEC0, RAS(*), DECS(*)
      REAL      RADIUS, PIXSIZ(2), SHIFT(2)
      INTEGER   IMSIZE(2), OVRLAP, OFIELD, IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXF
      PARAMETER (MAXF=20*MAXFLD)
C
      DOUBLE PRECISION COSDEC, SINDEC, COSFLD, SINFLD, RA(MAXF), XD,
     *   DEC(MAXF), RA2, DEC2, DX, DY, XRA, XDEC, XSH(2), LL, MM,
     *   RA0R, DEC0R, DRADD, DRAD
      REAL      DIST(MAXF)
      LOGICAL   THIS1
      INTEGER   IFIELD, MFIELD, II, JJ, INDX(MAXF), I1, I2, JERR, I3, I4
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL RFILL (MAXF, -1.0, DIST)
      COSDEC = COS (DEC0*DG2RAD)
      SINDEC = SIN (DEC0*DG2RAD)
      RA0R = RA0 * DG2RAD
      DEC0R = DEC0 * DG2RAD
      XSH(2) = SHIFT(2) / 3.6D3
      XSH(1) = SHIFT(1) / 3.6D3 / COSDEC
C                                       Spacing of imaging region (rad)
      DRAD = 0.5 * AS2RAD * PIXSIZ(1) * (IMSIZE(1) - OVRLAP)
      DX = 1.5 * DRAD
      DY = COS (30.0 * DG2RAD) * DRAD
C                                       field size in degrees
      DRADD = DRAD * RAD2DG / SQRT (2.0)
C                                       collect list
      IFIELD = 0
      MFIELD = 2.0 * SQRT (MAXF / PI)
      I2 = MFIELD/2 - 3
      I4 = I2 * (DX/DY) + 0.5
      I4 = (I4 / 2) * 2 - 1
      IF (MOD((2*I2+1)*I4+I2,2).EQ.1) I2 = I2 + 1
      I1 = -I2
      I3 = -I4
      THIS1 = .FALSE.
C                                       loop over declination
      DO 20 II = I1,I2
         MM = II * DY
C                                       loop over RA
         DO 10 JJ = I1,I2
            LL = JJ * DX
C                                       do every other possibility to
C                                       get hexagonal pattern
            THIS1 = .NOT.THIS1
            IF (THIS1) THEN
C                                       -> RA, Dec (-SIN projection)
               MSGSUP = 32000
               CALL NEWPOS (2, RA0R, DEC0R, LL, MM, XRA, XDEC, JERR)
               MSGSUP = 0
               IF (JERR.EQ.0) THEN
                  XRA = XRA * RAD2DG + XSH(1)
                  XDEC = XDEC * RAD2DG + XSH(2)
                  COSFLD = COS (XDEC*DG2RAD)
                  SINFLD = SIN (XDEC*DG2RAD)
                  XD = SINFLD * SINDEC +
     *               COSFLD * COSDEC * COS (XRA*DG2RAD - RA0R)
                  IF (XD.GT.1.0D0) THEN
                     XD = 0.0D0
                  ELSE
                     XD = RAD2DG * ACOS (XD)
                     END IF
                  IF (XD.LT.ABS(RADIUS)) THEN
                     IF (IFIELD.LT.MAXF) THEN
                        IFIELD = IFIELD + 1
                        RA(IFIELD) = XRA
                        DEC(IFIELD) = XDEC
                        DIST(IFIELD) = XD
                        INDX(IFIELD) = IFIELD
                     ELSE
                        MSGTXT = 'FLYEYE: NUMBER OF FIELDS EXCEEDED'
                        CALL MSGWRT (7)
                        IERR = 1
                        GO TO 999
                        END IF
                     END IF
                  END IF
               END IF
 10         CONTINUE
 20      CONTINUE
      MFIELD = IFIELD
C                                       sort on distances
      CALL BUBBLE (DIST, INDX, IFIELD, 1)
      I1 = IMSIZE(1) - OVRLAP
      I2 = IMSIZE(2) - OVRLAP
      OFIELD = 0
      DO 30 II = 1,MFIELD
         OFIELD = OFIELD + 1
         RA2 = RA(INDX(II))
         DEC2 = DEC(INDX(II))
         IF (RA2.LT.0.0D0) RA2 = RA2 + 360.0D0
         IF (RA2.GT.360.0D0) RA2 = RA2 - 360.0D0
         RAS(OFIELD) = RA2
         DECS(OFIELD) = DEC2
 30      CONTINUE
      WRITE (MSGTXT,1030) MFIELD, DIST(MFIELD)
      CALL MSGWRT (4)
      IF (DIST(MFIELD).LT.0.95*RADIUS) THEN
         MSGTXT = 'WARNING: THIS MAY NOT COVER THE DESIRED AREA'
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('FLYEYE found',I6,' fields to image to',F7.3,' deg')
      END
      SUBROUTINE ADNVSS (STD, MLUN, MIND, II, RAS, DECS, JJ, NCC, IRET)
C-----------------------------------------------------------------------
C   Searches the catalog for sources, adding those inside image II to
C   the image and the CC file (if this is the closest field).
C   Inputs:
C      STD      L      Standard input file
C      MLUN     I(2)   LUN of image file
C      MIND     I(2)   FTAB pointer of image file
C      II       I      Field number current image
C      RAS      D(*)   RA of center (degrees) all fields
C      DECS     D(*)   Dec of center (degrees) all fields
C      JJ       I      Current output field number
C   Output
C      NCC      I      Number Clean components
C      IRET     I      Error code
C-----------------------------------------------------------------------
      LOGICAL   STD
      DOUBLE PRECISION RAS(*), DECS(*)
      INTEGER   II, MLUN(2), MIND(2), JJ, NCC, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION SINDEC, COSDEC, SRA, XRA, XDEC, SINCAL, COSCAL,
     *   XD, LAMBD, XDMIN, SINFAC, COSFAC, FRA, XMIN, XMAX, XX, YY, ZZ
      REAL      XPIX, YPIX, XF, XB, PARMS(3), X, Y
      INTEGER   LFLUX, DEPTH(5), IERR, I, J, NS, CCLUN, VER, NCOL,
     *   CCKOLS(7), CCNUMV(7), CCRNO, CCTYPE, NMAX, NN, K, JT, JTRIM
      CHARACTER INLINE(25)*132
      LOGICAL   OUTSID, DONE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'FACES.INC'
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      LAMBD = VELITE / CATD(KDCRV+2)
      CCLUN = 20
      VER = 1
      NCOL = 3
      CCTYPE = 0
      CALL CCMINI ('WRIT', SCRTCH, ODISK, OCNO, VER, CATBLK, CCLUN,
     *   CCRNO, CCKOLS, CCNUMV, NCOL, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       init location common
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      NS = 0
      NCC = 0
      DONE = .FALSE.
C                                       find range
      IF (STD) THEN
         XMIN = 1.D4
         XMAX = -1.D4
         XPIX = CATBLK(KINAX)
         YPIX = CATBLK(KINAX+1)
         CALL XYVAL (1.0, 1.0, XX, YY, ZZ, J)
         XMIN = MIN (XMIN, XX)
         XMAX = MAX (XMAX, XX)
         CALL XYVAL (1.0, YPIX, XX, YY, ZZ, J)
         XMIN = MIN (XMIN, XX)
         XMAX = MAX (XMAX, XX)
         CALL XYVAL (XPIX, YPIX, XX, YY, ZZ, J)
         XMIN = MIN (XMIN, XX)
         XMAX = MAX (XMAX, XX)
         CALL XYVAL (XPIX, 1.0, XX, YY, ZZ, J)
         XMIN = MIN (XMIN, XX)
         XMAX = MAX (XMAX, XX)
         NMAX = 25
      ELSE
         NMAX = 1
         XMAX = 1.D4
         XMIN = -1.D4
         END IF
C                                       loop in file
 10   NN = 1
 11   CALL ZTXIO ('READ', LUNNV, INDNV, INLINE(NN), IRET)
      IF (IRET.EQ.2) THEN
         NMAX = NN
         IRET = 0
         DONE = .TRUE.
         GO TO 15
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         CALL MSGWRT (6)
         GO TO 999
      ELSE
         JT = JTRIM (INLINE(NN))
         IF (INLINE(NN)(1:1).EQ.';') GO TO 11
         NN = NN + 1
         IF (NN.LE.NMAX) GO TO 11
         NN = NMAX
         END IF
C                                       finished a buffer
 15   IF (NMAX.GT.1) THEN
         READ (INLINE(NMAX),1010) XRA
         IF (XRA.LT.XMIN) GO TO 10
         NMAX = 1
         END IF
C                                       empty buffer
      DO 30 K = 1,NN
         READ (INLINE(K),1010) XRA, XDEC, LFLUX
         IF (XRA.GT.XMAX) GO TO 900
C                                       in the image?
         CALL XYPIX (XRA, XDEC, XPIX, YPIX, IERR)
         IF ((IERR.EQ.0) .AND. (XPIX.GE.1.0) .AND. (YPIX.GE.1.0) .AND.
     *      (XPIX.LE.CATBLK(KINAX)) .AND. (YPIX.LE.CATBLK(KINAX+1)))
     *      THEN
            XDEC = XDEC * DG2RAD
            XRA = XRA * DG2RAD
            SINCAL = SIN (XDEC)
            COSCAL = COS (XDEC)
            XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA - SRA)
            IF (XD.GT.1.0D0) THEN
               XD = 0.0D0
            ELSE
               XD = RAD2DG * ACOS (XD)
               END IF
            CALL PBCALC (XD, LAMBD, ANAME, PBPARM(2), XB, OUTSID)
            IF (OUTSID) XB = 0.01
            XF = XB * LFLUX * BPARM(3) / 1000.0
            IF ((XB.GT.PBPARM(1)) .AND. (XF.GT.BFLUX)) THEN
C                                       add to image
               CALL ADDIMG (MLUN, MIND, BMAJ, XPIX, YPIX, XF, IRET)
               IF (IRET.NE.0) GO TO 999
               NS = NS + 1
C                                       Which image is closest?
               J = 0
               XDMIN = 1.E10
               DO 20 I = 1,OFIELD
                  SINFAC = SIN (DECS(I)*DG2RAD)
                  COSFAC = COS (DECS(I)*DG2RAD)
                  FRA = RAS(I) * DG2RAD
                  XD = SINCAL * SINFAC + COSCAL * COSFAC * COS(XRA-FRA)
                  IF (XD.GT.1.0D0) THEN
                     XD = 0.0D0
                  ELSE
                     XD = RAD2DG * ACOS (XD)
                     END IF
                  IF (ABS(XD).LT.XDMIN) THEN
                     XDMIN = ABS (XD)
                     J = I
                     END IF
 20               CONTINUE
C                                       Closest is this one
C                                       Add to CC file
               IF (J.EQ.II) THEN
                  NCC = NCC + 1
                  CCRNO = NCC
                  X = (XPIX - CATR(KRCRP)) * CATR(KRCIC)
                  Y = (YPIX - CATR(KRCRP+1)) * CATR(KRCIC+1)
                  CALL TABCCM ('WRIT', SCRTCH, CCRNO, CCKOLS, CCNUMV,
     *               NCOL, X, Y, XF, CCTYPE, PARMS, IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               END IF
            END IF
 30      CONTINUE
      IF (.NOT.DONE) GO TO 10
C
 900  IF ((NS.GT.0) .OR. (NCC.GT.0)) THEN
         IRET = 0
         WRITE (MSGTXT,1030) II, NS, NCC, JJ
         CALL MSGWRT (4)
      ELSE
         IRET = -1
         WRITE (MSGTXT,1031) II, NS, NCC
         CALL MSGWRT (2)
         END IF
      CALL TABCCM ('CLOS', SCRTCH, CCRNO, CCKOLS, CCNUMV, NCOL, X, Y,
     *   XF, CCTYPE, PARMS, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (F9.5,1X,F9.5,I7)
 1020 FORMAT ('ERROR',I5,' READING CATALOG FILE')
 1030 FORMAT ('Field',I5,':',I6,' sources,',I6,' CCs:',
     *   ' kept as output field',I4)
 1031 FORMAT ('Field',I5,':',I6,' sources,',I6,' CCs: discarded')
      END
      SUBROUTINE ADDIMG (MLUN, MIND, BMAJ, XPIX, YPIX, XF, IRET)
C-----------------------------------------------------------------------
C   Adds a Gaussian to the image
C   Inputs:
C      MLUN   I(2)   LUNs to use
C      MIND   I(2)   FTAB pointers
C      BMAJ   R      FWHM in pixels of Gaussian
C      XPIX   R      X pixel of Gaussian
C      YPIX   R      Y pixel of Gaussian
C      XF     R      Flux of Gaussian
C   Outputs:
C      IRET   I      I/O error code
C-----------------------------------------------------------------------
      INTEGER   MLUN(2), MIND(2), IRET
      REAL      BMAJ, XPIX, YPIX, XF
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IX, IY, IWIN(4), NX, NY, ILX, BUFSZ, BLKOF, BIND1,
     *   BIND2, X1, X2
      REAL      BUFF1(MABFSS), BUFF2(MABFSS), W, A, YOFF, DMX
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      BUFSZ = 2 * MABFSS
      BLKOF = 1
      W = 4 * LOG (2.0) / (BMAJ**2)
      DMX = CATR(KRDMX)
C                                       X parameters
      NX = CATBLK(KINAX)
      IWIN(1) = 1
      IWIN(3) = NX
      X1 = XPIX - 2*BMAJ
      X1 = MAX (X1, 1)
      X2 = XPIX + 2*BMAJ + 0.9
      X2 = MIN (X2, NX)
C                                       Y is trickier
      ILX = NBPS / (2 * NX)
      ILX = MAX (ILX, 1)
      NY = CATBLK(KINAX+1)
      IWIN(2) = YPIX - 2.*BMAJ - 1
      IWIN(2) = MAX (0, IWIN(2))
      IWIN(2) = (IWIN(2)/ILX) * ILX + 1
      IWIN(2) = MAX (0, IWIN(2))
      IWIN(4) = YPIX + 2.*BMAJ + 0.9
      IWIN(4) = ((IWIN(4)+ILX-1)/ILX) * ILX
      IWIN(4) = MIN (IWIN(4), NY)
C                                       open for read
      CALL MINIT ('READ', MLUN(1), MIND(1), NX, NY, IWIN, BUFF1, BUFSZ,
     *   BLKOF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT 1'
         GO TO 990
         END IF
C                                       open for write
      CALL MINIT ('WRIT', MLUN(2), MIND(2), NX, NY, IWIN, BUFF2, BUFSZ,
     *   BLKOF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT 2'
         GO TO 990
         END IF
C                                       loop over set of rows
      DO 50 IY = IWIN(2),IWIN(4)
         CALL MDISK ('READ', MLUN(1), MIND(1), BUFF1, BIND1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ ROW'
            GO TO 990
            END IF
         CALL MDISK ('WRIT', MLUN(2), MIND(2), BUFF2, BIND2, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITE ROW'
            GO TO 990
            END IF
         CALL RCOPY (NX, BUFF1(BIND1), BUFF2(BIND2))
         YOFF = (IY - YPIX) ** 2
         DO 30 IX = X1,X2
            A = W * ((IX-XPIX)**2 + YOFF)
            IF (A.LT.15.0) THEN
               BUFF2(BIND2+IX-1) = BUFF2(BIND2+IX-1) + XF * EXP (-A)
               DMX = MAX (DMX, BUFF2(BIND2+IX-1))
               END IF
 30         CONTINUE
 50      CONTINUE
C                                       finish the write
      CALL MDISK ('WRIT', MLUN(2), MIND(2), BUFF2, BIND2, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH WRITE'
         GO TO 990
         END IF
      CATR(KRDMX) = DMX
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADDIMG: ERROR',I5,' ON ',A)
      END
      SUBROUTINE FACEHI
C-----------------------------------------------------------------------
C   write HIstory info to FACES output images
C-----------------------------------------------------------------------
C
      INTEGER   LUNH, IERR, I
      CHARACTER HILINE*72
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'FACES.INC'
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
      LUNH = 21
      CALL HICREA (LUNH, ODISK, OCNO, CATBLK, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLASIN, SEQIN, IDISK, LUNH, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLSOUT, OSEQ, ODISK, LUNH, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       parms
      IF (BFLUX.GT.0.0) THEN
         WRITE (HILINE,1000) TSKNAM, BFLUX
         CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      WRITE (HILINE,1001) TSKNAM, BPARM(1)
      CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1002) TSKNAM, OVRLAP
      CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1004) TSKNAM, BPARM(3)
      CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,1003) TSKNAM, PBPARM(1)
      CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (PBPARM(2).GT.0.0) THEN
         DO 20 I = 3,7
            WRITE (HILINE,1005) TSKNAM, I, PBPARM(I)
            CALL HIADD (LUNH, HILINE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 200
 20         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUNH, .TRUE., SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A,'FLUX    =',F8.5,9X,'/ beam*source flux limit')
 1001 FORMAT (A,'BPARM(1)=',F7.2,10X,'/ search radius (deg)')
 1002 FORMAT (A,'BPARM(1)=',I7,10X,'/ overlap pixels')
 1003 FORMAT (A,'BPARM(1)=',F7.3,10X,'/ primary beam cutoff')
 1004 FORMAT (A,'BPARM(3)=',F7.3,10X,'/ scale factor on NVSS fluxes')
 1005 FORMAT (A,'PBPARM(',I1,')=',1PE11.4,7X,'/ beam pattern parameter')
      END
      SUBROUTINE BUBBLE (ARRAY1, ARRAY2, NUMBER, DIRECT)
C-----------------------------------------------------------------------
C   In place bubble sort with tracking of swap indices
C   Inputs:
C      NUMBER   I      Number of samples to sort
C      DIRECT   I      Direction of sort (-1 descending)
C   In/Out:
C      ARRAY1   R(*)   Array to sort
C      ARRAY2   I(*)   Array rearranged along with ARRAY1
C-----------------------------------------------------------------------
      REAL      ARRAY1(*)
      INTEGER   ARRAY2(*), NUMBER, DIRECT
C
      INTEGER   I, J, IT
      REAL      RT
      LOGICAL   DONE
C-----------------------------------------------------------------------
      DONE = .FALSE.
      I = 1
C                                       descending order sort
      IF (DIRECT.EQ.-1) THEN
 10      IF ((I.LT.NUMBER) .AND. (.NOT.DONE)) THEN
            DONE = .TRUE.
            DO 20 J = NUMBER,I+1,-1
               IF (ARRAY1(J).GT.ARRAY1(J-1)) THEN
                  RT = ARRAY1(J)
                  ARRAY1(J) = ARRAY1(J-1)
                  ARRAY1(J-1) = RT
                  IT = ARRAY2(J)
                  ARRAY2(J) = ARRAY2(J-1)
                  ARRAY2(J-1) = IT
                  DONE = .FALSE.
                  END IF
 20            CONTINUE
            I = I + 1
            GO TO 10
            END IF
C                                       ascending order sort
      ELSE
 30      IF ((I.LT.NUMBER) .AND. (.NOT.DONE)) THEN
            DONE = .TRUE.
            DO 40 J = NUMBER,I+1,-1
               IF (ARRAY1(J).LT.ARRAY1(J-1)) THEN
                  RT = ARRAY1(J)
                  ARRAY1(J) = ARRAY1(J-1)
                  ARRAY1(J-1) = RT
                  IT = ARRAY2(J)
                  ARRAY2(J) = ARRAY2(J-1)
                  ARRAY2(J-1) = IT
                  DONE = .FALSE.
                  END IF
 40            CONTINUE
            I = I + 1
            GO TO 30
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE SETCEL (RADIUS, IMSIZE, CELSIZ, IERR)
C-----------------------------------------------------------------------
C   SETCEL reads the data to determine the max baseline and W and
C   determines the appropriate cellsize and imsize
C   Inputs
C      RADIUS   R      Maximum radius to use (deg)
C   Output:
C      IMSIZE   I(2)   Image size to use
C      CELSIZ   R(2)   Cell size in
C      IERR     I      Error code: > 0 failed
C-----------------------------------------------------------------------
      INTEGER   IMSIZE(2), IERR
      REAL      RADIUS, CELSIZ(2)
C
      INTEGER   J
      REAL      B, W, R, RPARM(20), VIS(24), FS, HPBW, MAXFR
      LOGICAL   LIMIT
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       minimize data
      BCHAN = 1
      ECHAN = 1
      BIF = 1
      EIF = 1
      CALL UVGET ('INIT', RPARM, VIS, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) 'INIT', IERR
         CALL MSGWRT (8)
         GO TO 999
         END IF
      B = 0
      W = 0
C                                       read loop
 100  CALL UVGET ('READ', RPARM, VIS, IERR)
      IF (IERR.NE.-1) THEN
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
         R = RPARM(1+ILOCU)**2 + RPARM(1+ILOCV)**2
         B = MAX (B, R)
         W = MAX (W, ABS(RPARM(1+ILOCW)))
         GO TO 100
         END IF
C                                       close file
      CALL UVGET ('CLOS', RPARM, VIS, IERR)
      IF ((W.LE.0.0) .OR. (B.LE.0.0)) THEN
         IERR = 2
         MSGTXT = 'NO DATA FOUND: SET IMSIZE, CELLSIZE'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       set results
      B = SQRT (B)
C                                       min fringe spacing in asec
      FS = RAD2AS / B
C                                       basic cell FS/3
      FS = FS / 3.0
C                                       image size limited by W term
C                                       beam in radian
      HPBW = 3 * FS * AS2RAD
C                                       max field correct B/W
      MAXFR = SQRT (HPBW) / 3.0 * B / W
      R = RADIUS * DG2RAD
      LIMIT = R.LT.MAXFR
      IF (LIMIT) THEN
         MAXFR = R
         IF (((CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0)) .AND.
     *      ((IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0))) THEN
            CELSIZ(1) = FS
            CELSIZ(2) = FS
            END IF
         END IF
C                                       circular Clean
      J = 2 * MAXFR * RAD2AS / FS + 0.5
C                                       message
      WRITE (MSGTXT,1010) J, FS
      CALL MSGWRT (4)
C                                       now set IMSIZE given CELL
      IF ((CELSIZ(1).GT.0.0) .AND. (CELSIZ(2).GT.0.0)) THEN
         J = 2 * MAXFR * RAD2AS / CELSIZ(1) + 10.5
         IF (J.GT.11200) THEN
            IMSIZE(1) = 16384
         ELSE IF (J.GT.5600) THEN
            IMSIZE(1) = 8192
         ELSE IF (J.GT.2800) THEN
            IMSIZE(1) = 4096
         ELSE IF (J.GT.1400) THEN
            IMSIZE(1) = 2048
         ELSE IF (J.GT.700) THEN
            IMSIZE(1) = 1024
         ELSE IF (J.GT.350) THEN
            IMSIZE(1) = 512
         ELSE IF (J.GT.175) THEN
            IMSIZE(1) = 256
         ELSE IF (J.GT.87) THEN
            IMSIZE(1) = 128
         ELSE
            IMSIZE(1) = 64
            END IF
         IMSIZE(2) = IMSIZE(1)
C                                       or set CELL given IMSI
      ELSE IF ((IMSIZE(1).GT.0) .AND. (IMSIZE(2).GT.0)) THEN
         CELSIZ(1) = 2. * MAXFR * RAD2AS / (IMSIZE(1) - 10.)
         CELSIZ(2) = CELSIZ(1)
C                                       balance the two
      ELSE
         J = 2 * MAXFR * RAD2AS / FS + 20.5
         IF (J.GT.9200) THEN
            IMSIZE(1) = 16384
         ELSE IF (J.GT.4600) THEN
            IMSIZE(1) = 8192
         ELSE IF (J.GT.2300) THEN
            IMSIZE(1) = 4096
         ELSE IF (J.GT.1150) THEN
            IMSIZE(1) = 2048
         ELSE IF (J.GT.575) THEN
            IMSIZE(1) = 1024
         ELSE IF (J.GT.282) THEN
            IMSIZE(1) = 512
         ELSE IF (J.GT.141) THEN
            IMSIZE(1) = 256
         ELSE IF (J.GT.70) THEN
            IMSIZE(1) = 128
         ELSE
            IMSIZE(1) = 64
            END IF
         IMSIZE(2) = IMSIZE(1)
         CELSIZ(1) = 2. * MAXFR * RAD2AS / (IMSIZE(1) - 20.)
         CELSIZ(2) = CELSIZ(1)
         END IF
C                                       message
      WRITE (MSGTXT,1020) IMSIZE(1), CELSIZ(1)
      CALL MSGWRT (4)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UV DATA ',A,' ERROR',I5)
 1010 FORMAT ('SETCEL: recommends IMSIZE',I6,' CELLSIZE',F10.5)
 1020 FORMAT ('SETCEL: returns    IMSIZE',I6,' CELLSIZE',F10.5)
      END

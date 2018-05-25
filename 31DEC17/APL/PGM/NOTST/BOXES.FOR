LOCAL INCLUDE 'BOXES.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLASI(2), XXSOUR(4,30), XBOX(12), XOBOX(12),
     *   XINFIL(12)
      REAL       DISKIN, XSEQ, CELSIZ(2), XSIZE(2), XFIELD,
     *   XRASH(MAXAFL), XDECSH(MAXAFL), BFLUX, BPARM(10), PBPARM(7)
      COMMON /INPARM/ XNAMEI, XCLASI, XSEQ, DISKIN, XXSOUR, XBOX,
     *   XOBOX, CELSIZ, XSIZE, XFIELD, XRASH, XDECSH, BFLUX, BPARM,
     *   PBPARM, XINFIL
C
      DOUBLE PRECISION RA0, DEC0
      INTEGER   IMSIZE(2), OVRLAP, CATNO, SEQIN, IDISK, LUNNV, INDNV,
     *   OFIELD, SCRTCH(512), LUNBOX, INDBOX
      CHARACTER NAMEIN*12, CLASIN*6, NVSSF*48, MSOURC*16, BOXFIL*48,
     *   OBXFIL*48
      COMMON /BOXESP/ RA0, DEC0, SCRTCH, IMSIZE, OVRLAP, CATNO, SEQIN,
     *   IDISK, LUNNV, INDNV, LUNBOX, INDBOX, OFIELD
      COMMON /BOXESC/ NAMEIN, CLASIN, NVSSF, MSOURC, BOXFIL, OBXFIL
      INCLUDE 'INCS:DANT.INC'
LOCAL END
      PROGRAM BOXES
C-----------------------------------------------------------------------
C! Make Clean boxes from NVSS
C# Imaging Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2002-2003, 2005, 2009, 2011-2012
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
C   BOXES makes a file which can then be used as an input BOXFILE
C   for IMAGR.  This is intended for wide-field imaging nd to cope with
C   interfering sources including the Sun.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME       XNAMEI/NAMEIN    Name of input UV data.
C      INCLASS      XCLASI/CLASIN    Class of input UV data.
C      INSEQ        SEQ/SEQIN        Seq. of input UV data.
C      INDISK       DISKIN/IDISK     Disk number of input UV data.
C      SOURCES      XXSOUR/SOURCE    Source which will be mapped.
C      BOXFILE      BOXFIL           BOXFILE input file name.
C      OBOXFILE     OBXFIL           BOXFILE output file name.
C      CELLSIZE     CELSIZ           pixel size in image.
C      IMSIZE       XSIZE/IMSIZE     image size, also field size.
C      SHIFT        SHIFT            offset for all fields.
C      BPARM        BPARM            1. Cbox radius pixels
C                                    2. > 0 Keep Cboxes from before
C                                    3. Flux scale factor
C                                    4. Max field number to change
C      PBPARM       PBPARM           Primary beam cutoff, flag, parms
C      INFILE       XINFIL/NVSSF     NVSS input file name.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER PNAM*6, MTYPE*2, STAT*4, EXT*4, TYPE*2, ZTXO*4,
     *   ANVSSF*48
      INTEGER   NPARMS, IRET, IERR, IROUND, LUNSU, SQUAL, SOUID, NID,
     *   FREQAX, II, CTTUV(256), NCC, IVER, I
      REAL      RADIUS, CTTUVR(256), CATR(256), AFLUX
      DOUBLE PRECISION LAMBD, RAS(MAXFLD), DECS(MAXFLD), CTTUVD(128),
     *   CATD(128), DTEMP
      HOLLERITH CTTUVH(256), CATH(256)
      LOGICAL   NVOPEN, STD, MULTI
      INCLUDE 'BOXES.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATH, CATD, CATR, CATBLK)
      EQUIVALENCE (CTTUV, CTTUVR, CTTUVH, CTTUVD)
      INCLUDE 'INCS:PSTD.INC'
      DATA PNAM /'BOXES '/
      DATA LUNSU /27/
      DATA NID, SQUAL /1, -1/
      DATA MTYPE /'UV'/
C-----------------------------------------------------------------------
C                                       initialize
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      NCFILE = 0
      ZTXO = 'READ'
      NCC = 0
C                                       get the input parameters
      NPARMS = 186 + 2 * MAXAFL
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
      IF (BPARM(1).LE.1.0) BPARM(1) = 3.0
      IF (BPARM(2).LE.0.0) BPARM(2) = 1.5
      IF ((PBPARM(1).LE.0.0) .OR. (PBPARM(1).GT.0.9)) PBPARM(1) = 0.023
      IF (BPARM(3).LE.0.0) BPARM(3) = 1.0
      IF (BPARM(5).LE.0.0) BPARM(5) = MAXFLD
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLASI, CLASIN)
      CALL H2CHR (48, 1, XINFIL, NVSSF)
      CALL H2CHR (48, 1, XBOX, BOXFIL)
      CALL H2CHR (48, 1, XOBOX, OBXFIL)
      SEQIN = IROUND (XSEQ)
      IDISK = IROUND (DISKIN)
C                                       locate uv file in directory
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
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      CALL TABIO ('CLOS', 0, 1, ANBUFF, ANBUFF, IERR)
C                                       look for an SU table
      CALL MULSDB (CATBLK, MULTI)
C                                       single-source use UVPGET output
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
      IF ((CELSIZ(1).LE.0.0) .OR. (CELSIZ(2).LE.0.0) .OR.
     *   (IMSIZE(1).LE.0) .OR. (IMSIZE(2).LE.0)) THEN
         MSGTXT = 'CELLSIZE AND IMSIZE MUST BE SPECIFIED'
         IRET = 8
         GO TO 980
         END IF
C                                       Copy BOXFIL if any
      OFIELD = XFIELD + 0.5
      DO 10 I = 1,OFIELD
         RAS(I) = XRASH(I)
         DECS(I) = XDECSH(I)
 10      CONTINUE
      CALL BOXCOP (OFIELD, RAS, DECS, RADIUS, IRET)
      IF (IRET.NE.0) GO TO 990
      RADIUS = RADIUS + MAX (CELSIZ(1)*IMSIZE(1), CELSIZ(2)*IMSIZE(2)) /
     *   3600.0
      RADIUS = 1.2 * RADIUS
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
C                                       make a subset file
      STD = NVSSF(:8).EQ.'AIPSTARS'
      IF (DEC0+RADIUS.GT.90.0D0) STD = .FALSE.
      IF (DEC0-RADIUS.LT.-90.0D0) STD = .FALSE.
      IF (STD) THEN
         DTEMP = RADIUS / COS (DG2RAD * DEC0)
         IF (RA0-DTEMP.LT.0.0D0) STD = .FALSE.
         IF (RA0+DTEMP.GT.360.0D0) STD = .FALSE.
         END IF
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
      CATR(KRBMJ) = 2.0 * BPARM(1) * CATR(KRCIC+1)
      CATR(KRBMN) = CATR(KRBMJ)
      CALL CHR2H (12, NAMEIN, KHIMNO, CATH(KHIMN))
      TYPE = 'MA'
      CALL CHR2H (2, TYPE, KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
C                                       loop over output fields
      DO 100 II = 1,OFIELD
         CATD(KDCRV) = RAS(II)
         CATD(KDCRV+1) = DECS(II)
         CATD(KDORA) = RA0
         CATD(KDODE) = DEC0
         CATR(KRDMX) = 0.0
         CATR(KRDMN) = 0.0
C                                       open catalog
         CALL ZTXOPN (ZTXO, LUNNV, INDNV, ANVSSF, .FALSE., IERR)
         ZTXO = 'QRED'
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 980
            END IF
         NVOPEN = .TRUE.
C                                       catalog sources to image
         CALL ADNVSS (STD, II, NCC, IRET)
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
 100     CONTINUE
      GO TO 990
C                                       Error
 980  CALL MSGWRT (8)
C                                       close files and go to bed.
 990  IF (NVOPEN) CALL ZTXCLS (LUNNV, INDNV, IERR)
      IF (ANVSSF.NE.NVSSF) CALL ZTXZAP (LUNNV, ANVSSF, IERR)
      CALL ZTXCLS (LUNBOX, INDBOX, IERR)
      WRITE (MSGTXT,1990) NCC
      CALL MSGWRT (4)
C                                       close files and go to bed.
 995  CALL DIETSK (IRET, RQUICK, SCRTCH)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('CATIO ERROR NO', I5)
 1010 FORMAT ('Found ', A12, A6, ' Seq', I4, ' Disk:', I3,' in slot',
     *   I5)
 1040 FORMAT ('ERROR NO', I5, ' OPENING NVSS FILE')
 1050 FORMAT ('FIELD',I6,' ERROR',I5,' ADDING NVSS TO IMAGE FILE')
 1060 FORMAT ('ERROR NO', I5, ' CLOSING NVSS FILE')
 1990 FORMAT ('Wrote a total of',I5,' Clean boxes')
      END
      SUBROUTINE BOXCOP (MFIELD, RAS, DECS, RADIUS, IRET)
C-----------------------------------------------------------------------
C   BOXCOP reads in the BOXFILE, copying selected contents to OBOXFILE
C   and picking up the coordinates of each field.
C   In/out
C      MFIELD   I      In - number values in RAS/DECS
C                      Out - Max field number to use
C      RAS      D(*)   In - RAshift, out - RA of each field
C      DECS     D(*)   in - Decshift, Out - Dec of each field
C   Outputs:
C      RADIUS   R      Maximum radius for fields
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   MFIELD, IRET
      DOUBLE PRECISION RAS(*), DECS(*)
      REAL      RADIUS
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'BOXES.INC'
      INTEGER   I, I1, KBP, J, JTRIM
      CHARACTER LINE*132, SIGN*1
      DOUBLE PRECISION X, SINDEC, COSDEC, SRA, XD, XRA, XDEC, COSCAL,
     *   SINCAL
      REAL      RASH, DECSH, XR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'C
C-----------------------------------------------------------------------
C                                       init values
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      RADIUS = 0.0
      IF (MFIELD.LE.0) THEN
         MFIELD = 1
         DECS(1) = DEC0
         RAS(1) = RA0
         CALL DFILL (MAXFLD, -180.0D0, DECS)
C                                       change shifts to centers
      ELSE
         DO 5 I = 1,MFIELD
            RASH = RAS(I)
            DECSH = DECS(I)
            CALL XYSHFT (RA0, DEC0, RASH, DECSH, 0.0, XRA, XDEC)
            RAS(I) = XRA
            DECS(I) = XDEC
            XDEC = XDEC * DG2RAD
            XRA = XRA * DG2RAD
            SINCAL = SIN (XDEC)
            COSCAL = COS (XDEC)
            XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA-SRA)
            IF (XD.GT.1.0D0) THEN
               XR = 0.0D0
            ELSE
               XR = RAD2DG * ACOS (XD)
               END IF
            RADIUS = MAX (RADIUS, XR)
 5          CONTINUE
         I = MAXFLD - MFIELD
         CALL DFILL (I, -180.0D0, DECS(MFIELD+1))
         END IF
C                                       open output file
      LUNBOX = 3
      CALL ZTXOPN ('WRIT', LUNBOX, INDBOX, OBXFIL, .FALSE., IRET)
      IF (IRET.NE.0) GO TO 999
C                                       copy the file
      IF (BOXFIL.NE.' ') THEN
         CALL ZTXOPN ('READ', LUNNV, INDNV, BOXFIL, .FALSE., IRET)
         IF (IRET.NE.0) GO TO 999
C                                       read loop
 10      CALL ZTXIO ('READ', LUNNV, INDNV, LINE, IRET)
C                                       line read
         IF (IRET.EQ.0) THEN
            CALL CHTRIM (LINE, 132, LINE, I)
C                                       #, fldsiz, rash, decsh
            IF ((LINE(:1).EQ.'F') .OR. (LINE(:1).EQ.'f')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.BPARM(5))) THEN
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  RASH = X
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  DECSH = X
                  CALL XYSHFT (RA0, DEC0, RASH, DECSH, 0.0, XRA, XDEC)
                  RAS(I1) = XRA
                  DECS(I1) = XDEC
                  MFIELD = MAX (MFIELD, I1)
                  XDEC = XDEC * DG2RAD
                  XRA = XRA * DG2RAD
                  SINCAL = SIN (XDEC)
                  COSCAL = COS (XDEC)
                  XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA-SRA)
                  IF (XD.GT.1.0D0) THEN
                     XR = 0.0D0
                  ELSE
                     XR = RAD2DG * ACOS (XD)
                     END IF
                  RADIUS = MAX (RADIUS, XR)
                  END IF
C                                       parse for ra/dec field
C                                       # fldsiz ra (HH MM SS) Dec (+-
C                                       DD MM SS)
            ELSE IF ((LINE(:1).EQ.'C') .OR. (LINE(:1).EQ.'c')) THEN
               KBP = 2
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
               IF ((I1.GE.1) .AND. (I1.LE.BPARM(5))) THEN
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) GO TO 990
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
 40               IF (J.GT.132) GO TO 990
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
                  RAS(I1) = XRA
                  DECS(I1) = XDEC
                  MFIELD = MAX (MFIELD, I1)
                  XDEC = XDEC * DG2RAD
                  XRA = XRA * DG2RAD
                  SINCAL = SIN (XDEC)
                  COSCAL = COS (XDEC)
                  XD = SINCAL * SINDEC + COSCAL * COSDEC * COS(XRA-SRA)
                  IF (XD.GT.1.0D0) THEN
                     XR = 0.0D0
                  ELSE
                     XR = RAD2DG * ACOS (XD)
                     END IF
                  RADIUS = MAX (RADIUS, XR)
                  END IF
C                                       clean box
            ELSE IF ((LINE(:1).GE.'0') .AND. (LINE(:1).LE.'9')) THEN
               KBP = 1
               CALL GETNUM (LINE, 132, KBP, X)
               IF (X.EQ.DBLANK) GO TO 10
               IF (X.GE.0.0D0) THEN
                  I1 = X + 0.50D0
               ELSE
                  I1 = X - 0.50D0
                  END IF
C                                       count/skip this box
               IF ((I1.GE.1) .AND. (I1.LE.BPARM(5))) THEN
                  MFIELD = MAX (MFIELD, I1)
                  IF (BPARM(4).LE.0.0) GO TO 10
                  END IF
               END IF
C                                       copy the line
            I = JTRIM (LINE)
            CALL ZTXIO ('WRIT', LUNBOX, INDBOX, LINE(:I), IRET)
            IF (IRET.NE.0) GO TO 999
            GO TO 10
C                                       end of file
         ELSE IF (IRET.EQ.2) THEN
            CALL ZTXCLS (LUNNV, INDNV, IRET)
            END IF
         END IF
      GO TO 999
C
 990  MSGTXT = 'ERROR PARSING INPUT CARD'
      CALL MSGWRT (8)
      MSGTXT = LINE(:80)
      CALL MSGWRT (8)
C
 999  RETURN
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
     *   NMAX, NN, I
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
         I = JTRIM (INLINE(NN))
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
      SUBROUTINE ADNVSS (STD, II, NCC, IRET)
C-----------------------------------------------------------------------
C   Searches the catalog for sources, adding those inside image II to
C   the image and the CC file (if this is the closest field).
C   Inputs:
C      STD      L      Standard input file
C      II       I      Field number current image
C   IN/Output
C      NCC      I      Number Clean components running total
C   Output
C      IRET     I      Error code
C-----------------------------------------------------------------------
      LOGICAL   STD
      INTEGER   II, NCC, IRET
C
      INCLUDE 'INCS:PSTD.INC'
      DOUBLE PRECISION SINDEC, COSDEC, SRA, XRA, XDEC, SINCAL, COSCAL,
     *   XD, LAMBD, XMIN, XMAX, XX, YY, ZZ
      REAL      XPIX, YPIX, XF, XB, WIDTH
      INTEGER   LFLUX, DEPTH(5), IERR, NS, J, JTRIM, NMAX, NN, I, JT
      CHARACTER INLINE(25)*132, LINE*30
      LOGICAL   OUTSID, DONE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'BOXES.INC'
      DATA DEPTH /5*1/
C-----------------------------------------------------------------------
      SINDEC = SIN (DEC0*DG2RAD)
      COSDEC = COS (DEC0*DG2RAD)
      SRA = RA0 * DG2RAD
      LAMBD = VELITE / CATD(KDCRV+2)
C                                       init location common
      LOCNUM = 1
      CALL SETLOC (DEPTH, .FALSE.)
      NS = 0
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
         J = JTRIM (INLINE(I))
         IF (J.LT.36) THEN
            READ (INLINE(I),1010) XRA, XDEC, LFLUX
            WIDTH = BPARM(1)
         ELSE
            READ (INLINE(I),1010) XRA, XDEC, LFLUX, WIDTH
            WIDTH = ABS (WIDTH) * BPARM(2) / MAX (CELSIZ(1), CELSIZ(2))
            END IF
         IF (XRA.GT.XMAX) GO TO 900
C                                       in the image?
         CALL XYPIX (XRA, XDEC, XPIX, YPIX, IERR)
         IF ((IERR.EQ.0) .AND. (XPIX-WIDTH.GE.1.0) .AND.
     *      (YPIX-WIDTH.GE.1.0) .AND.
     *      (XPIX+WIDTH.LE.CATBLK(KINAX)) .AND.
     *      (YPIX+WIDTH.LE.CATBLK(KINAX+1))) THEN
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
C                                       add to BOXFILE
            IF ((XB.GT.PBPARM(1)) .AND. (XF.GT.BFLUX)) THEN
               WRITE (LINE,1015) II, WIDTH, XPIX, YPIX
               JT = JTRIM (LINE)
               CALL ZTXIO ('WRIT', LUNBOX, INDBOX, LINE(:JT), IRET)
               IF (IRET.NE.0) GO TO 999
               NCC = NCC + 1
               NS = NS + 1
               END IF
            END IF
 20      CONTINUE
      IF (.NOT.DONE) GO TO 10
C
 900  IF (NS.GT.0) THEN
         WRITE (MSGTXT,1900) II, NS
         CALL MSGWRT (4)
         END IF
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (F9.5,1X,F9.5,I7,F10.4)
 1015 FORMAT (I3,' -1',3F8.1)
 1020 FORMAT ('ERROR',I5,' READING CATALOG FILE')
 1900 FORMAT ('Field',I5,':',I6,' boxes')
      END

LOCAL INCLUDE 'SDMOD.INC'
C                                       Local include for SDMOD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAI2(2), XNAMOU(3),
     *   XCLAOU(2), XOPTYP
      REAL      XSIN, XDISIN, XSI2, XDISI2, XSOUT, XDISO, XBCH, XECH,
     *   XBIF, XEIF, XGAUSS, GMAX(4), GPOS(2,4), GWID(3,4), APARM(10),
     *   FLUX, BPARM(10), BUFF1(UVBFSS), BUFF2(UVBFSS), DFJ(4), DFN(4),
     *   DMAX(4), DDX(4), DDC(4), DDY(4), DFF(4), THROW
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, BIF, EIF,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, NGAUSS, BCH, ECH, SEQIN2, DISKI2,
     *   UCAT(256), ICAT(256)
      LOGICAL   ISCOMP, ISFREQ, BSCONT, FIXRA, FIXDEC
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAIN2*6, NAMOUT*12,
     *   CLAOUT*6, OPTYPE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XCLAI2,
     *   XSI2, XDISI2, XNAMOU, XCLAOU, XSOUT, XDISO, XOPTYP, XBCH, XECH,
     *   XBIF, XEIF, XGAUSS, GMAX, GPOS, GWID, APARM, FLUX, BPARM
      COMMON /SDMODC/ CATOLD, UCAT, ICAT, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, NGAUSS, DFJ, DFN, DFF, DMAX,
     *   DDX, DDC, DDY, BCH, ECH, BIF, EIF, ISFREQ, BSCONT, THROW,
     *   FIXRA, FIXDEC, SEQIN2, DISKI2
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAIN2, NAMOUT, CLAOUT,
     *   OPTYPE
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
C                                       End local include for SDMOD
LOCAL END
      PROGRAM SDMOD
C-----------------------------------------------------------------------
C! Adds a model to single-dish "uv" data sets
C# Modeling singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000, 2005, 2008, 2012, 2015
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
C   SDMOD adds a model to single-dish uv data or replaces the data with
C   the model plus noise.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      NGAUSS         XGAUSS        Number gaussian components
C      GMAX           GMAX          Peak value
C      GPOS           GPOS          Ra, dec offsets
C      GWIDTH         GWIDTH        major, minor, pa
C      FLUX           FLUX          < 0 -> add model in to data
C                                   >= 0 replace data with model +
C                                   noise w rms = FLUX
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NX, NY
      REAL      IMAGE(2)
      LONGINT   IPTR
      INCLUDE 'SDMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'SDMOD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SDMOIN (PRGM, NX, NY, IPTR, IMAGE, IRET)
C                                       Call routine that alters data
      IF (IRET.EQ.0) CALL SDMOUV (NX, NY, IMAGE(IPTR), IRET)
C                                       history
      IF (IRET.EQ.0) CALL SDMOHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SDMOIN (PRGN, NX, NY, IPTR, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SDMOIN gets input parameters for SDMOD and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6     Program name
C      IMAGE   R(2)    Address base for dynamic memory of image
C   Output:
C      NX      I       Number x points in image
C      NY      I       Number y points in image
C      IPTR    LI      Index into Image for start of dynamic memory
C      IRET    I       Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(*)
      LONGINT   IPTR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, LOCS, KEYTYP,
     *   DEPTH(5), IY, WIN(4), IBLKOF, MLUN, MIND, BIND
      LONGINT   IP
      LOGICAL   T, F
      INCLUDE 'SDMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK, DEPTH  /'      ', 5*1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 72
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAI2, CLAIN2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      SEQIN2 = IROUND (XSI2)
      DISKIN = IROUND (XDISIN)
      DISKI2 = IROUND (XDISI2)
      DISKO = IROUND (XDISO)
      NGAUSS = XGAUSS + 0.5
      IF ((NGAUSS.LT.1) .AND. (OPTYPE.NE.'IMAG')) THEN
         MSGTXT = 'SPECIFY SOME GAUSSIANS PLEASE'
         IRET = 2
         GO TO 990
         END IF
      BSCONT = BPARM(1).GT.0.0
      IF (ABS(BPARM(2)).LE.0.1) BPARM(2) = 1
      FIXRA = BPARM(4).GT.0.0
      FIXDEC = BPARM(5).GT.0.0
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READING HEADER'
         GO TO 990
         END IF
C                                       Get throw
      THROW = 0.0
      IF (BSCONT) THEN
         CALL CATKEY ('READ', DISKIN, OLDCNO, 'BSTHROW', 1, LOCS, THROW,
     *      KEYTYP, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'GETTING THROW'
            GO TO 990
            END IF
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      IF ((TYPUVD.NE.1) .AND. (TYPUVD.NE.3)) THEN
         MSGTXT = 'I ONLY WORK ON SINGLE-DISH UNPROJECTED DATA'
         IRET = 2
         GO TO 990
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       limit channels, IFs
      IF ((JLOCIF.GE.0) .AND. (CATBLK(KINAX+JLOCIF).GT.1)) THEN
         BIF = XBIF + 0.1
         IF ((BIF.LT.1) .OR. (BIF.GT.CATBLK(KINAX+JLOCIF))) BIF = 1
         EIF = XEIF + 0.1
         IF ((EIF.LT.BIF) .OR. (EIF.GT.CATBLK(KINAX+JLOCIF))) EIF =
     *      CATBLK(KINAX+JLOCIF)
         CATBLK(KINAX+JLOCIF) = EIF - BIF + 1
         CATR(KRCRP+JLOCIF) = CATR(KRCRP+JLOCIF) - BIF + 1.0
      ELSE
         BIF = 1
         EIF = 1
         END IF
      IF ((JLOCF.GE.0) .AND. (CATBLK(KINAX+JLOCF).GT.1)) THEN
         BCH = XBCH + 0.1
         IF ((BCH.LT.1) .OR. (BCH.GT.CATBLK(KINAX+JLOCF)) .OR. (BSCONT))
     *      BCH = 1
         ECH = XECH + 0.1
         IF ((ECH.LT.BCH) .OR. (ECH.GT.CATBLK(KINAX+JLOCF)) .OR.
     *      (BSCONT)) ECH = CATBLK(KINAX+JLOCF)
         CATBLK(KINAX+JLOCF) = ECH - BCH + 1
         CATR(KRCRP+JLOCF) = CATR(KRCRP+JLOCF) - BCH + 1.0
      ELSE
         BCH = 1
         ECH = 1
         END IF
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      IRET = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1000) IERR, 'CREATING OUTPUT FILE'
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            MSGTXT = 'MAY OVERWRITE INPUT FILE ONLY.  QUITTING'
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'UPDATING NEW HEADER'
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                       Get throw
      IF (BSCONT) THEN
         CALL CATKEY ('WRIT', DISKO, CCNO, 'BSTHROW', 1, LOCS, THROW,
     *      KEYTYP, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING THROW'
            GO TO 990
            END IF
         END IF
C                                       Save output file info
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      SEQOUT = CATBLK(KIIMS)
      CALL COPY (256, CATBLK, UCAT)
C                                       Get CATBLK of image file
      NX = 0
      NY = 0
      IPTR = 1
      IF (OPTYPE.EQ.'IMAG') THEN
         NGAUSS = 0
         OLDCNO = 1
         PTYPE = 'MA'
         MLUN = 16
         CALL MAPOPN ('READ', DISKI2, NAME2, CLAIN2, SEQIN2, PTYPE,
     *      NLUSER, MLUN, MIND, OLDCNO, CATBLK, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1030) IERR, NAME2, CLAIN2, SEQIN2, DISKI2,
     *         NLUSER
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKI2
         FCNO(NCFILE) = OLDCNO
         FRW(NCFILE) = 0
         LOCNUM = 1
         CALL SETLOC (DEPTH, .FALSE.)
         CALL COPY (256, CATBLK, ICAT)
         NX = CATBLK(KINAX)
         NY = CATBLK(KINAX+1)
         LOCS = (NX * NY - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', TSKNAM, LOCS, IMAGE, IPTR, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'ALLOCATING IMAGE MEMORY'
            GO TO 990
            END IF
         IPTR = IPTR + 1
         WIN(1) = 1
         WIN(2) = 1
         WIN(3) = NX
         WIN(4) = NY
         IP = IPTR
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), DEPTH, IBLKOF, IERR)
         IBLKOF = IBLKOF + 1
C                                       Initialize for I/O
         CALL MINIT ('READ', MLUN, MIND, NX, NY, WIN, BUFF1, JBUFSZ,
     *      IBLKOF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT IMAGE I/O'
            GO TO 990
            END IF
C                                       Copy plane to memory
         DO 20 IY = 1,NY
            CALL MDISK ('READ', MLUN, MIND, BUFF1, BIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READING IMAGE'
               GO TO 990
               END IF
            CALL RCOPY (NX, BUFF1(BIND), IMAGE(IP))
            IP = IP + NX
 20         CONTINUE
         CALL MAPCLS ('READ', DISKI2, OLDCNO, MLUN, MIND, CATBLK, F,
     *      BUFF1, IERR)
         NCFILE = NCFILE - 1
         END IF
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOIN: ERROR',I3,1X,A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
      END
      SUBROUTINE SDMOUV (NX, NY, IMAGE, IRET)
C-----------------------------------------------------------------------
C   SDMOUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NX, NY, IRET
      REAL      IMAGE(NX,NY)
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'SDMOD.INC'
      REAL      DUM, BASEN, CBUFF(UVBFSS), RESULT(UVBFSS)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / UCAT(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call user routine: compressed
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL SDMOMO (NUMVIS, BUFF1(IPTRI+ILOCU),
     *            BUFF1(IPTRI+ILOCV), CBUFF, INCX, NX, NY, IMAGE,
     *            RESULT, IRET)
C                                       Un compressed data
            ELSE
               CALL SDMOMO (NUMVIS, BUFF1(IPTRI+ILOCU),
     *            BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+NRPRMI), INCX, NX, NY,
     *            IMAGE, RESULT, IRET)
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       Continue w or w/o output
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Final call to SDMOMO.
 200  NUMVIS = -1
      CALL SDMOMO (NUMVIS, DUM, DUM, BUFF1, INCX, NX, NY, IMAGE,
     *   RESULT, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, UCAT, BUFF2, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('SDMOUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SDMOUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SDMOUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SDMOUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SDMOUV: SDMOMO ERROR',I3)
 1150 FORMAT ('SDMOUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SDMOHI
C-----------------------------------------------------------------------
C   SDMOHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      REAL      XJ, XN, UCATR(256)
      LOGICAL   T
      INCLUDE 'SDMOD.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (UCAT, UCATR)
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), UCAT, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      IF (OPTYPE.EQ.'IMAG') THEN
         CALL HENCO2 (TSKNAM, NAME2, CLAIN2, SEQIN2, DISKI2, LUN2,
     *      BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       inputs:
      IF (JLOCF.GE.0) THEN
         WRITE (HILINE,1010) TSKNAM, BCH, ECH
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (JLOCIF.GE.0) THEN
         WRITE (HILINE,1011) TSKNAM, BCH, ECH
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (FLUX.GE.0.0) THEN
         WRITE (HILINE,1012) TSKNAM, FLUX
      ELSE
         WRITE (HILINE,1013) TSKNAM
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       components
      IF (NGAUSS.GT.0) THEN
         WRITE (HILINE,1014) TSKNAM, NGAUSS
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         HILINE = TSKNAM // '/ Pre-convolution Gaussian parameters:'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         DO 20 I = 1,NGAUSS
            WRITE (HILINE,1015) TSKNAM, I, GMAX(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            WRITE (HILINE,1016) TSKNAM, I, GPOS(1,I), GPOS(2,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            WRITE (HILINE,1017) TSKNAM, I, GWID(1,I), GWID(2,I),
     *         GWID(3,I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            IF (ISFREQ) THEN
               IF (APARM(2*I).LE.0.0) THEN
                  HILINE = TSKNAM // '/ independent of channel'
               ELSE
                  WRITE (HILINE,1018) TSKNAM, I, APARM(2*I-1),
     *               APARM(2*I)
                  END IF
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 20         CONTINUE
         IF ((ECH.GT.BCH) .AND. (.NOT.ISFREQ)) THEN
            HILINE = TSKNAM // '/ all independent of channel'
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
         IF (UCATR(KRBMJ).GT.0.0) THEN
            HILINE = TSKNAM // '/ Convolved Gaussian actually added:'
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            DO 30 I = 1,NGAUSS
               WRITE (HILINE,1015) TSKNAM, I, DMAX(I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               WRITE (HILINE,1016) TSKNAM, I, GPOS(1,I), GPOS(2,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               IF ((DFJ(I).GT.0.0) .AND. (DFN(I).GT.0.0)) THEN
                  XJ = SQRT (4.*LOG(2.) / DFJ(I))
                  XN = SQRT (4.*LOG(2.) / DFN(I))
                  WRITE (HILINE,1017) TSKNAM, I, XJ, XN, GWID(3,I)
                  CALL HIADD (LUN2, HILINE, BUFF2, IERR)
                  IF (IERR.NE.0) GO TO 200
                  END IF
 30            CONTINUE
            END IF
      ELSE
         HILINE = TSKNAM // '/ Model = IN2NAME image'
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Continuum
      IF (BSCONT) THEN
         WRITE (HILINE,1030) TSKNAM, BPARM(2)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         WRITE (HILINE,1031) TSKNAM, BPARM(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (FIXRA) THEN
         WRITE (HILINE,1035) TSKNAM, BPARM(4)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (FIXDEC) THEN
         WRITE (HILINE,1036) TSKNAM, BPARM(5)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (BPARM(6).NE.0.0) THEN
         WRITE (HILINE,1037) TSKNAM, BPARM(6)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF (BPARM(7).NE.0.0) THEN
         WRITE (HILINE,1038) TSKNAM, BPARM(7)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      IF ((FIXRA) .OR. (FIXDEC) .OR. (BPARM(6).NE.0.) .OR.
     *   (BPARM(7).NE.0.0)) THEN
         IF (BPARM(8).GT.0.0) THEN
            HILINE = TSKNAM // '/ coordinate corrected before modeling'
         ELSE
            HILINE = TSKNAM // '/ coordinate corrected after modeling'
            END IF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), UCAT, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), UCAT, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SDMOHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'BCHAN=',I5,'  ECHAN=',I5)
 1011 FORMAT (A6,'BIF  =',I5,'  EIF  =',I5)
 1012 FORMAT (A6,'NOISE=',F8.3,5X,'/ Noise added, data discarded')
 1013 FORMAT (A6,'/ Data retained, noiseless model subtracted')
 1014 FORMAT (A6,'NGAUSS=',I2,10X,'/ Gaussians used')
 1015 FORMAT (A6,'GMAX(',I1,')=',1PE13.5,10X,'/ Peak brightness')
 1016 FORMAT (A6,'GPOS(',I1,')=',F8.1,',',F8.1,6X,
     *   '/ X,Y center in arc seconds')
 1017 FORMAT (A6,'GWID(',I1,')=',F6.1,',',F6.1,',',F6.1,3X,
     *   '/ major,minor arc sec, PA deg')
 1018 FORMAT (A6,'GCHN(',I1,')=',F6.1,',',F5.1,11X,
     *   '/ center,fwhm spectral channels')
 1030 FORMAT (A6,'SCALE =',F8.4,5X,'/ change throw length')
 1031 FORMAT (A6,'ROTATE=',F8.4,5X,'/ rotate throw ccw (degrees)')
 1035 FORMAT (A6,'XINC =',F8.4,5X,'/ RA/Az increment in arcsec')
 1036 FORMAT (A6,'YINC =',F8.4,5X,'/ Dec/El increment in arcsec')
 1037 FORMAT (A6,'XSHIFT =',F8.4,5X,'/ RA/Az shift in arcsec')
 1038 FORMAT (A6,'YSHIFT =',F8.4,5X,'/ Dec/El shift in arcsec')
 1200 FORMAT ('SDMOHI: ERROR COPYING TABLES')
      END
      SUBROUTINE SDMOMO (NUMVIS, U, V, VIS, INCX, NX, NY, IMAGE,
     *   RESULT, IRET)
C-----------------------------------------------------------------------
C   Modifies the record to add model to data or to replace data with
C   model plus noise.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths - actually RA here
C      V       R    V in wavelengths - actually Dec here
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C      NX      I    Image x dimension
C      NY      I    Image y dimension
C   Inputs from COMMON:
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      LRECI      I    Input file record length
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   In/Out
C      IMAGE    R(NX,NY)   Image for modeling (read 1st time)
C   Output:
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, NX, NY, IRET
      REAL      U, V, VIS(INCX,*), RESULT(INCX,*), IMAGE(NX,NY)
C
      INTEGER   JIF, JF, JS, NS, INDEXO, INDEXI, I, IROUND, IERR, I1,
     *   I2, J1, J2, J
      REAL      X, Y, XJ, XN, MODEL, Z, ZMODEL, XINT, UCATR(256), XP,
     *   YP, S, COSD
      DOUBLE PRECISION RRA, RDEC, UCATD(128), XRA, XDEC
      SAVE RRA, RDEC
      INCLUDE 'SDMOD.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (UCAT, UCATR, UCATD)
C-----------------------------------------------------------------------
      IRET = 0
C                                       Init information about
C                                       Gaussians:
      IF (NUMVIS.EQ.1) THEN
         IF (FLUX.GT.0.0) CALL RANDIN (I)
         X = UCATR(KRBMJ) * 3600
         IF (.NOT.BSCONT) THEN
            RRA = UCATD(KDORA)
            RDEC = UCATD(KDODE)
         ELSE
            RRA = -THROW * COS (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
            RDEC= -THROW * SIN (DG2RAD * BPARM(3)) * BPARM(2) / 3600.
            END IF
C                                       the SD beam had better be round
         ISFREQ = .FALSE.
C                                       Precompute gaussians
         IF (NGAUSS.GT.0) THEN
            DO 10 I = 1,NGAUSS
               IF (APARM(2*I).GT.0.0) ISFREQ = .TRUE.
               IF ((X.LE.0.0) .AND. ((GWID(1,I).LE.0.0) .OR.
     *            (GWID(2,I).LE.0.0))) THEN
                  WRITE (MSGTXT,1000) I
                  CALL MSGWRT (8)
                  IRET = 8
                  GO TO 999
                  END IF
               DFJ(I) = 4. * LOG(2.0) / (X*X + GWID(1,I)*GWID(1,I))
               DFN(I) = 4. * LOG(2.0) / (X*X + GWID(2,I)*GWID(2,I))
               DFF(I) = 0.0
               IF (APARM(2*I).GT.0.0) DFF(I) = 4. * LOG(2.0) /
     *            (APARM(2*I)**2)
               DMAX(I) = GMAX(I)
               IF ((X.GT.0.0) .AND. (GWID(1,I).GT.0.0) .AND.
     *            (GWID(2,I).GT.0.0)) DMAX(I) = DMAX(I) * GWID(1,I) *
     *            GWID(2,I) / SQRT ((X*X + GWID(1,I)**2) * (X*X +
     *            GWID(2,I)**2))
               DDY(I) = RDEC + GPOS(2,I)/3600.
               DDC(I) = COS (DG2RAD * DDY(I))
               DDX(I) = RRA * DDC(I) + GPOS(1,I) / 3600
 10            CONTINUE
            IF ((ECH.LE.BCH) .OR. (BSCONT)) ISFREQ = .FALSE.
            END IF
         END IF
C                                       Current record
      IF (NUMVIS.GT.0) THEN
C                                       correct coords in advance
         IF (BPARM(8).GT.0.0) THEN
            IF (FIXDEC) THEN
               XINT = BPARM(5) / 3600.
               V = XINT * IROUND (V / XINT)
               END IF
            V = V + BPARM(7) / 3600.0
            COSD = COS (DG2RAD * V)
            IF (FIXRA) THEN
               IF (COSD.NE.0.0) THEN
                  XINT = BPARM(4) / 3600. / COSD
                  U = XINT * IROUND (U / XINT)
               ELSE
                  U = 0.0
                  END IF
               END IF
            IF (COSD.NE.0.0) U = U + BPARM(6) / 3600.0 / COSD
            END IF
C                                       find flux
         IF (.NOT.ISFREQ) THEN
            MODEL = 0.
            IF (NGAUSS.GT.0) THEN
               DO 110 I = 1,NGAUSS
                  X = U * DDC(I) - DDX(I)
                  Y = V - DDY(I)
                  Z = DG2RAD*GWID(3,I)
                  XJ = (X * SIN (Z) + Y * COS (Z)) * 3600.
                  XN = (Y * SIN (Z) - X * COS (Z)) * 3600.
                  Z = DFJ(I) * XJ * XJ + DFN(I) * XN * XN
                  IF (Z.LT.10.) MODEL = MODEL + DMAX(I) * EXP (-Z)
 110              CONTINUE
            ELSE
               XRA = U
               XDEC = V
               IF (BSCONT) THEN
                  XRA = XRA - RRA
                  XDEC = XDEC - RDEC
                  END IF
               CALL XYPIX (XRA, XDEC, XP, YP, IERR)
               IF ((XP.GT.2.0) .AND. (XP.LT.NX-1) .AND. (YP.GT.2.0)
     *            .AND. (YP.LT.NY-1)) THEN
                  I1 = XP - 1.0
                  I2 = I1 + 3
                  J1 = YP - 1.0
                  J2 = J1 + 3
                  S = 0.0
                  DO 120 J = J1,J2
                     X = ABS (YP - J) * PI
                     IF (X.EQ.0.0) THEN
                        Y = 1.0
                     ELSE
                        Y = SIN(X) / X
                        END IF
                     DO 115 I = I1,I2
                        X = ABS (XP - I) * PI
                        IF (X.EQ.0.0) THEN
                           X = Y
                        ELSE
                           X = Y * SIN(X) / X
                           END IF
                        IF (IMAGE(I,J).NE.FBLANK) THEN
                           S = S + X
                           MODEL = MODEL + X * IMAGE(I,J)
                           END IF
 115                    CONTINUE
 120                 CONTINUE
                  IF (S.NE.0.0) MODEL = MODEL / S
                  END IF
               END IF
            END IF
         NS = 1
         IF (JLOCS.GE.0) NS = UCAT(KINAX+JLOCS)
         DO 170 JIF = BIF,EIF
            DO 155 JS = 1,NS
               ZMODEL = 0
               IF (.NOT.ISFREQ) ZMODEL = MODEL
               INDEXI = (JIF-BIF) * INCIFI + (JS-1) * INCSI + 1 +
     *            (BCH - 1) * INCFI
               INDEXO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
               DO 130 JF = BCH,ECH
                  DO 125 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
 125                 CONTINUE
                  IF ((JF.EQ.1) .OR. (.NOT.BSCONT)) THEN
                     IF (FLUX.LT.0) THEN
                        RESULT(1,INDEXO) = VIS(1,INDEXI) - ZMODEL
                     ELSE IF (FLUX.EQ.0.0) THEN
                        RESULT(1,INDEXO) = ZMODEL
                     ELSE
                        CALL NOISE (X)
                        RESULT(1,INDEXO) = ZMODEL + FLUX * X
                        END IF
                     END IF
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 130              CONTINUE
C                                       freq-dependent model
               IF (ISFREQ) THEN
                  DO 150 I = 1,NGAUSS
                     X = U * DDC(I) - DDX(I)
                     Y = V - DDY(I)
                     Z = DG2RAD*GWID(3,I)
                     XJ = (X * SIN (Z) + Y * COS (Z)) * 3600.
                     XN = (Y * SIN (Z) - X * COS (Z)) * 3600.
                     Z = DFJ(I) * XJ * XJ + DFN(I) * XN * XN
                     IF (Z.LT.10.) THEN
                        ZMODEL = DMAX(I) * EXP (-Z)
                        INDEXO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
                        IF (APARM(2*I).LE.0.0) THEN
                           DO 135 JF = BCH,ECH
                              RESULT(1,INDEXO) = RESULT(1,INDEXO) +
     *                           ZMODEL
                              INDEXO = INDEXO + INCFO
 135                          CONTINUE
                        ELSE
                           X = APARM(2*I-1)
                           DO 140 JF = BCH,ECH
                              Y = Z + DFF(I) * (JF - X) * (JF - X)
                              IF (Y.LT.10) RESULT(1,INDEXO) =
     *                           RESULT(1,INDEXO) + DMAX(I) * EXP (-Y)
                              INDEXO = INDEXO + INCFO
 140                          CONTINUE
                           END IF
                        END IF
 150                 CONTINUE
                  END IF
 155           CONTINUE
 170        CONTINUE
C                                       correct coords after the fact
         IF (BPARM(8).LE.0.0) THEN
            IF (FIXDEC) THEN
               XINT = BPARM(5) / 3600.
               V = XINT * IROUND (V / XINT)
               END IF
            V = V + BPARM(7) / 3600.0
            COSD = COS (DG2RAD * V)
            IF (FIXRA) THEN
               IF (COSD.NE.0.0) THEN
                  XINT = BPARM(4) / 3600. / COSD
                  U = XINT * IROUND (U / XINT)
               ELSE
                  U = 0.0
                  END IF
               END IF
            IF (COSD.NE.0.0) U = U + BPARM(6) / 3600.0 / COSD
            END IF
C                                       last call - history
      ELSE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('COMPONENT',I2,' MUST HAVE WIDTH OR HEADER BEAM WIDTH')
      END
      SUBROUTINE NOISE (ANOISE)
C-----------------------------------------------------------------------
C   Random noise generator
C    Output: ANOISE  R    Result
C-----------------------------------------------------------------------
      REAL      ANOISE, TEMP
      INTEGER   J
C-----------------------------------------------------------------------
      ANOISE = -6.0
      DO 10 J = 1,12
         CALL RANDUM (TEMP)
         ANOISE = ANOISE + TEMP
 10      CONTINUE
C
 999  RETURN
      END

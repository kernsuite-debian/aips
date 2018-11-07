LOCAL INCLUDE 'UVLOD.INC'
C                                       Local include for UVLOD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      DOUBLE PRECISION PSCAL(20), POFF(20), BSC, BZE, WTSCAL,
     *   BXSTA(50), BYSTA(50), BZSTA(50)
      HOLLERITH XNAMEI(2), XNAMOU(3), XCLAOU(2), XBAND, XINFIL(12)
      CHARACTER NAMEIN*8, NAMOUT*12, CLAOUT*6, BAND*4, INFILE*48,
     *   NAMSTA(50)*8, PTYPES(40)*8
      REAL      DOALL, BITER, NITER, QUAL, TAPE, DOEOF, XFILES, XDOUVC,
     *   DOKEEP, XOUTS, DISO, XVSMAX, XPIECE, XERROR, BUFF2(UVBFSL),
     *   PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
      INTEGER   IBLANK, NUMVIS, BUFF1(UVBFSL), FDVEC(50), TBIND, LENREC,
     *   IQUAL, NFILES, DISOUT, TAPEIN, GROUP, ICEND, TABLES, IBPP,
     *   NPARMS, ITAB(20), JADR(4), NSTACT, JBAND, SCRBUF(256), KLOCWT,
     *   TAPBUF(29184), UNKNWN, IPIECE, NPIECE, NPV(4), CONCAT, INITVS,
     *   OUTSEQ, USED(300), CATSAV(256), DPIECE, NZERO(3,3), PBASE,
     *   PANT1, PANT2, NSKIP
      LOGICAL   NDOEOF, ISBLNK, LCMPLX, LSTOKE, STDEXT, DODISK, DOUVCM,
     *   UVTABL
      COMMON /BUFRS/ TAPBUF, BUFF2, BUFF1, SCRBUF
      COMMON /INPARM/ DOALL, XNAMEI, QUAL, XBAND, BITER, NITER, TAPE,
     *   XFILES, XINFIL, XDOUVC, DOEOF, DOKEEP, XNAMOU, XCLAOU, XOUTS,
     *   DISO, XVSMAX, XPIECE, XERROR
      COMMON /SCRINF/ NUMVIS, NDOEOF, IQUAL, NFILES, DISOUT,
     *   TAPEIN, LENREC, FDVEC, TBIND, JBAND, INITVS
      COMMON /FITINF/ CATSAV, PSCAL, POFF, BSC, BZE, WTSCAL, GROUP,
     *   UNKNWN, ICEND, IBLANK, TABLES, IBPP, NPARMS, ITAB, JADR,
     *   NSTACT, KLOCWT, DODISK, ISBLNK, LCMPLX, LSTOKE, STDEXT, DOUVCM,
     *   UVTABL, IPIECE, NPIECE, NPV, CONCAT, OUTSEQ, PCMATX, CDMATX,
     *   PVMATX, USED, DPIECE, NZERO, NSKIP, PBASE, PANT1, PANT2
      COMMON /STACOM/ BXSTA, BYSTA, BZSTA
      COMMON /CHRCOM/ NAMEIN, NAMOUT, CLAOUT, BAND, INFILE, NAMSTA,
     *   PTYPES
LOCAL END
      PROGRAM UVLOD
C-----------------------------------------------------------------------
C! Read uv data from FITS or VLA Export format file
C# UV Tape FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2001, 2003-2004, 2007-2013, 2015-2017
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
C   UVLOD will load a uv data base from a VLA export tape or an AIPS/
C   Charlottesville IBM FITS tape (and possibly other FITS tapes) and
C   catalog them on disk.  A VLB version of the Export format is also
C   supported.  This version extends the RUN structure (1) to indicate
C   that antenna locations must be multiplied by 1000, (2) possibly to
C   give 4 frequencies rather than 4 Stokes, and (3) to give various
C   timing values.
C   Inputs:
C      Adverb  Pgm. name         Comments.
C      DOALL     DOALL      Load all sources in Export fmt file
C      SOURCE    NAMEIN     Source name. (Ignored on FITS)
C      QUAL      IQUAL      Source qualifier. (Ignored on FITS)
C      BAND      JBAND      Observing band (eq. 'L','C').  Ignored
C                           when reading FITS.
C      BCOUNT    BITER      Sequence # of first source to include
C      NCOUNT    NITER      Number of sequential source to load
C      INTAPE    TAPEIN     Input tape drive number
C      NFILES    NFILES     Number of files to skip.
C      INFILE    INFILE     Disk file name for FITS file
C      DCONCAT   NDOEOF     > 0 => concatanate all selected data in
C                           one file (DOALL must be false)
C      OUTNAME   NAMOUT     Output file name (default = NAMEIN)
C      OUTCLASS  CLAOUT     Output file class (default = 'UVDATA')
C      OUTSEQ    OUTSEQ     Output sequence number requested (<0 =>
C                           use one found on FITS tape header)
C      OUTDISK   DISOUT     Output disk volumn.
C      NPOINTS   XVSMAX     No. thousands of vis. (Ignored on FITS)
C      NPIECE    NPIECE     Max piece number to be read.
C   Adverbs SOURCE, QUAL, BAND, BCOUNT, NCOUNT, and NPOINTS do not
C   apply to FITS format data.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER PRGM*6
      INTEGER   ISLOT, IERR, IHLUN, JERR
      LOGICAL   FITS, EOF, LAST
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'UVLOD '/
      DATA IHLUN /27/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL UVPARM (PRGM, FITS, IERR)
      IF (IERR.NE.0) GO TO 900
      UNKNWN = 0
      DPIECE = 0
C                                       Read vis tape, copy to
C                                       catalogd file.
C                                       VLA Export format
      IF (.NOT.FITS) THEN
         CALL VISIN (IERR)
C                                       FITS of some sort
      ELSE
         NSKIP = 0
 10      IPIECE = IPIECE + 1
         IF (IPIECE.LE.NPIECE) THEN
            LAST = IPIECE.EQ.NPIECE
            TABLES = 0
            CALL FILL (300, 0, USED)
            CALL COPY (256, CATBLK, CATSAV)
            CALL UVFHDR (ISLOT, IERR)
            IF (IERR.LT.0) GO TO 800
            IF (IERR.NE.0) GO TO 850
            CALL PCHDR (PCMATX, CDMATX, PVMATX)
            CALL UVFHIS (ISLOT, IHLUN, IERR)
            IF (IERR.NE.0) GO TO 850
            CALL FILL (6, 0, NZERO)
            IF (.NOT.UVTABL) THEN
               CALL UVFDAT (ISLOT, IERR)
               IF (IERR.NE.0) GO TO 850
               END IF
C                                       Try new table extension files
            CALL FITRXU (ISLOT, IHLUN, LAST, EOF, IERR)
            IF (IERR.NE.0) GO TO 850
C                                       Old table format.
            IF (.NOT.EOF) THEN
               CALL UVFEXT (ISLOT, IHLUN, IERR)
               IF (IERR.NE.0) GO TO 850
               END IF
            CONCAT = 1
            GO TO 10
            END IF
C                                       source table
         CALL SUCHCK (DISOUT, ISLOT)
C                                       index table
         CALL NXCHCK (DISOUT, ISLOT, BUFF2)
C                                       file ending reads
 800     IF (UNKNWN.GT.0) THEN
            WRITE (MSGTXT,1800) UNKNWN
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Close input
 850  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, JERR)
C                                       Finished
 900  CALL DIE (IERR, SCRBUF)
C
 999  STOP
C-----------------------------------------------------------------------
 1800 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
      END
      SUBROUTINE UVPARM (PRGM, FITS, JERR)
C-----------------------------------------------------------------------
C   UVPARM reads input parameters for Task UVLOD.  See header comments
C   in PROGRAM UVLOD for more details.
C   Inputs: PRGM   C*6      Program name
C   Output: FITS   L        T => FITS tape
C           JERR   I        Error code: 0 => ok, else quit
C   Common: /MAPHDR/ initial cat block header (heavily filled in if
C                    FITS is false)
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, KBAND(7)*1, IRAN(5)*8, ICOOR(5)*8, CHTMP*400,
     *   TRYTWO*48
      LOGICAL   FITS, T
      INTEGER   NAXIS, NRAN, IFIL, JERR, IERR, I, INDEXX, IRET, IROUND,
     *   IT, J, NBYT, ITRIM
      REAL      EPS
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T /.TRUE./
      DATA KBAND /'L','C','U','K','P','X','S'/
      DATA IRAN /'UU-L    ','VV-L    ','WW-L    ','BASELINE','TIME1   '/
      DATA ICOOR /'COMPLEX ','STOKES  ','FREQ    ','RA      ',
     *   'DEC     '/
      DATA NAXIS, NRAN /5,5/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL HIINIT (3)
      CALL VHDRIN
      CALL FILL (50, 0, FDVEC)
      JERR = 0
      BSC = 1.0D0
      BZE = 0.0D0
C                                       Initialize COMMON /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARMS = 34
      CALL GTPARM (PRGM, NPARMS, RQUICK, DOALL, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRET = 8
         GO TO 20
C                                       Convert characters
 10   CALL H2CHR (8, 1, XNAMEI, NAMEIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XBAND, BAND)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      OUTSEQ = XOUTS + 0.5
C                                       Compressed output?
      DOUVCM = XDOUVC.GT.0.0
C                                       Check if disk file output
      DODISK = INFILE.NE.' '
      CALL CATINI (CATBLK)
      CALL FILL (49, 0.0, PCMATX)
      CALL FILL (49, 0.0, PVMATX)
      CALL FILL (49, 0.0, CDMATX)
      EPS = 0.1
      IPIECE = 0
      NPIECE = IROUND (XPIECE)
C                                       Get TAPEIN
      TAPEIN = TAPE + EPS
C                                       Get NFILES
      NFILES = XFILES + EPS
      IF (XFILES.LT.-EPS) NFILES = XFILES - EPS
C                                       Get DISOUT
      DISOUT = DISO + EPS
C                                       Get max. no. visibilities.
      IF (XVSMAX.LE.0.) XVSMAX = 50.
      XVSMAX = XVSMAX * 1200.0D0
C                                       Set user id in CATBLK
      CATBLK(KIIMU) = NLUSER
C                                       If RQUICK, restart AIPS
      IRET = 0
C                                       Test legality
      IF (NTAPED.EQ.1) TAPEIN = 1
      IF (((TAPEIN.GE.1) .AND. (TAPEIN.LE.NTAPED)) .OR. DODISK) GO TO 15
         WRITE (MSGTXT,1010) TAPEIN
         CALL MSGWRT (8)
         IRET = 8
         GO TO 20
 15   IF (((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) .OR. DODISK)
     *    GO TO 20
         WRITE (MSGTXT,1015)
         CALL MSGWRT (8)
         IRET = 8
 20   JERR = IRET
      IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
C                                       Setup for Tape I/O
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
      CALL CHR2H (48, INFILE, 1, FDVEC(7))
C                                       Disk input.
      TRYTWO = ' '
      IF (DODISK) THEN
         FDVEC(1) = 25
         TAPEIN = 1
         FDVEC(5) = 1
         MSGSUP = 32000
         CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.EQ.0) THEN
            CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
         ELSE
            TRYTWO = INFILE
            I = ITRIM (INFILE)
            INFILE(I+1:) = '1'
            CALL CHR2H (48, INFILE, 1, FDVEC(7))
            END IF
         MSGSUP = 0
C                                       Tape input
      ELSE
         TAPEIN = TAPE + 0.5
         IF (TAPEIN.LE.0) TAPEIN = 1
         FDVEC(1) = 129 - TAPEIN
         FDVEC(5) = TAPEIN
         FDVEC(6) = 10
         WRITE (MSGTXT,1024) TAPEIN
         CALL MSGWRT (6)
         END IF
C                                       Open tape
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.GT.1) THEN
         IF (TRYTWO.NE.' ') THEN
            MSGTXT = 'ALSO COULD NOT OPEN ' // TRYTWO
            CALL MSGWRT (8)
            END IF
         WRITE (MSGTXT,1025) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 999
         END IF
C                                       Skip to correct file.
      IERR = 0
      IF ((NFILES.GT.0) .AND. (.NOT.DODISK))
     *   CALL ZTAPE ('ADVF', FDVEC(1), FDVEC(40), NFILES, IERR)
      IFIL = 1 - NFILES
      IF ((NFILES.LT.0) .AND. (.NOT.DODISK))
     *   CALL ZTAPE ('BAKF', FDVEC(1), FDVEC(40), IFIL, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1026) IERR
         CALL MSGWRT (8)
         JERR = 8
         GO TO 990
         END IF
C                                       Read first tape rec
      MSGSUP = 32000
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, JERR)
      MSGSUP = 0
      NBYT = FDVEC(42)
      IF ((JERR.NE.0) .AND. (JERR.NE.10)) THEN
         WRITE (MSGTXT,1030) JERR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Look for "SIMPLE  = "
      CALL ZC8CL (400, 1, TAPBUF, CHTMP)
      FITS = CHTMP(:10).EQ.'SIMPLE  = '
      IF (FITS) THEN
         IF (JERR.NE.10) THEN
            IT = INDEX (CHTMP(:80), '/')
            IF (IT.LE.0) IT = 80
            DO 40 J = 11,IT
               IF (CHTMP(J:J).NE.' ') THEN
                  IF (CHTMP(J:J).NE.'T') JERR = 10
                  GO TO 45
                  END IF
 40            CONTINUE
            JERR = 10
            END IF
 45      IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1045) NBYT
            CALL MSGWRT (8)
            GO TO 990
            END IF
         UVTABL = (CHTMP(188:190).EQ.'  2') .AND.
     *      (CHTMP(348:350).EQ.'  0') .AND.
     *      (CHTMP(262:268).EQ.'7777777')
         CONCAT = -1
         IF (.NOT.UVTABL) THEN
            NPIECE = 1
         ELSE
            IF (NPIECE.LE.0) NPIECE = 999
            IF (DOEOF.GT.0.0) CONCAT = 0
            END IF
C                                       EXPORT format
      ELSE
         NPIECE = 1
         JERR = 0
         CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) THEN
            JERR = IERR
            WRITE (MSGTXT,1035) JERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
C                                       set LRECL
         FDVEC(2) = 4008
         FDVEC(6) = 1
C                                       Variable length records
         FDVEC(31) = 1
C                                       Prepare header for VLA Export
         IF (DOALL.GT.0.0) DOEOF = -1.0
         NDOEOF = DOEOF.LE.0.0
         IF (BITER.LT.1.0) BITER = 1.0
         IF ((NITER.LT.1.0) .AND. (DOALL.GT.0.0)) NITER = 10.0
         IF (NITER.LT.1.0) NITER = 100.0
C                                       Get Source qualifier.
         IQUAL = IROUND (QUAL)
C                                       Get band.
         JBAND = 0
         DO 50 I = 1,7
            IF (BAND(1:1).EQ.KBAND(I)) JBAND = I
 50         CONTINUE
         JBAND = JBAND - 1
C                                       Fill CATBLK.
C                                       Set units.
         CHTMP = 'JY'
         CALL CHR2H (8, CHTMP, 1, CATH(KHBUN))
C                                       Random para. types.
         DO 60 I = 1,NRAN
            INDEXX = KHPTP + (I-1)*2
            CALL CHR2H (8, IRAN(I), 1, CATH(INDEXX))
 60         CONTINUE
         CATBLK(KIPCN) = NRAN
         CATR(KRCIC) = 1.0
         CATR(KRCIC+1) = -1.0
         CATD(KDCRV) = 1.0D0
         CATD(KDCRV+1) = -1.0D0
C                                       Axis types.
         DO 70 I = 1,NAXIS
            INDEXX = KHCTP + (I-1)*2
            CALL CHR2H (8, ICOOR(I), 1, CATH(INDEXX))
 70         CONTINUE
         CATBLK(KIDIM) = NAXIS
C                                       Axis dimensions.
         CATBLK(KINAX) = 3
         CATBLK(KINAX+1) = 4
         CATBLK(KINAX+2) = 1
         CATBLK(KINAX+3) = 1
         CATBLK(KINAX+4) = 1
C                                       Set file type to 'UV'
         CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
         END IF
      GO TO 999
C
 990  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVPARM: ERROR',I3,' OBTAINING INPUT PARAMETERS ')
 1010 FORMAT ('UVPARM: INTAPE =',I7,' ILLEGAL')
 1015 FORMAT ('TAPE JOBS ARE NOT ALLOWED IN BATCH')
 1024 FORMAT ('Reading tape drive number ',I3)
 1025 FORMAT ('ERROR',I7,' OPENING TAPE')
 1026 FORMAT ('ERROR',I7,' POSITIONING TAPE')
 1030 FORMAT ('ERROR',I7,' READING FIRST TAPE RECORD')
 1035 FORMAT ('ERROR',I7,' RE-POSITIONING TAPE WITH BAKF(1)')
 1045 FORMAT ('NON-STANDARD FITS HEADER OF',I7,' BYTES READ, QUITTING')
      END
      SUBROUTINE VISIN (JERR)
C-----------------------------------------------------------------------
C   VISIN reads an EXPORT visibility tape, finds the requested data and
C   copies it into a cataloged file.
C   Inputs (via COMMON /SRCINF/):
C      NAMEIN           C*8  Source name
C      IQUAL            I    Source qualifier
C      JBAND            I    Source band number.
C      TAPEIN           I    Tape drive number
C      NFILES           I    No. tape files to skip.
C      XVSMAX           R    Max. no. visibilities.
C   from /BUFRS/
C      BUFF1(9220)      I    Work buffer
C      BUFF2(9220)      R    Work buffer
C      TAPBUF(*)        I    Tape buffer
C   Output:
C      JERR             I    Return error code.
C                            0 = OK
C                            5 = Did not find requested data.
C                            8 = I/O error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SOURCE*8, OBSDAT*11, DEFNAM*12, DEFCLS*6, LOCNAM*12,
     *   LOCCLS*6, TYPES(6)*4, PNAME*48, IBAND(7)*1, CHTEMP*8
      INTEGER   BLKOF, MJAD, VO, ITEMP(8), ITYPES(2,6), JERR, LUNO,
     *   INDO, LENBU, I2(4), SZBUFF, MXXANT, LQUAL, IW(2), I, NRPARM,
     *   NUMANT, ANVER, IDISO, IERR, IN, INDEXX, IOCNT, IOFF, IS, ITYP,
     *   KIND, NCOR, NIOUT, NWD, SPAT(8)
      LOGICAL   T, F, FIRST, GOTDAT, WANTED, EQUAL, STARTD, GANDAT,
     *   DOFRQ, DOVLB, DONONE, ISNAME, WSNAME
      REAL      TIMADD, BASADD, XIAT, XUT1, SCUV, SCUVN, SCVIS, XCNT,
     *   XREC, YREC, NUMSOR, R4
      DOUBLE PRECISION FREQ, F8, XMJAD, TIMFAC, XDAT, STFAC, GST0
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (F8, R4, I2(1))
      DATA TYPES /'VIS ','SOUR','ANTE','DEF ','RUN ','END '/
      DATA T, F /.TRUE.,.FALSE./
      DATA IBAND /'P','L','C','U','K','X','S'/
      DATA NRPARM /5/
      DATA LUNO, LENBU, BLKOF, VO /16, 128, 1, 0/
C-----------------------------------------------------------------------
      TIMFAC = 1.0D0 / 65536.0D0
      IOCNT = 0
      NUMSOR = 0.0
      WANTED = F
      STARTD = F
      GOTDAT = F
      DONONE = F
      DOVLB = F
      DOFRQ = F
      XIAT = 0.0
      XUT1 = 0.0
      GST0 = 0.0D0
      MXXANT = 0
      NUMANT = 0
      JERR = 0
      CCNO = 0
      GANDAT = F
      ANVER = 0
      TIMADD = 0.0
      BASADD = 0.0
      XREC = 0.0
      ISNAME = NAMEIN.NE.' '
      IF (ISNAME) CALL PSFORM (8, NAMEIN, SPAT)
      I = INDEX (NAMEIN, '*')
      IF (I.LE.0) I = INDEX (NAMEIN, '?')
      WSNAME = ISNAME .AND. (I.LE.0)
C                                       Convert data to mess on tape.
      IN = -3
      DO 10 I = 1,6
         IN = IN + 4
         CALL ZCLC8 (4, TYPES(I), IN, BUFF2)
 10      CONTINUE
      CALL ZI16IL (12, 1, BUFF2, ITYPES)
      FIRST = T
      NUMVIS = 0
C                                       Begin loop reading tape records
 60   CONTINUE
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         XREC = XREC + 1.0
         IF (IERR.EQ.0) GO TO 70
         IF (IERR.EQ.4) GO TO 900
            WRITE (MSGTXT,1060) IERR, XREC
            CALL MSGWRT (8)
            JERR = 8
            GO TO 990
C                                       Flip bytes off tape.
 70      CALL ZI16IL (1, 1, TAPBUF(TBIND), IN)
         NWD = IN / 2
         IF ((NWD.GT.0) .AND. (NWD.LE.2004)) GO TO 75
            WRITE (MSGTXT,1070) NWD
            CALL MSGWRT (8)
            JERR = 8
            GO TO 990
 75      CALL ZI16IL (NWD, 1, TAPBUF(TBIND), BUFF1)
C                                       Is this first tape block?
         IF (.NOT.FIRST) GO TO 90
C                                       Find first record type.
            IW(1) = BUFF1(5)
            IW(2) = BUFF1(6)
            ITYP = 7
            DO 80 I = 1,6
               IF ((IW(1).EQ.ITYPES(1,I)) .AND. (IW(2).EQ.ITYPES(2,I)))
     *            ITYP = I
 80            CONTINUE
C                                       Set record pointer.
            IS = 15
            FIRST = F
            GO TO 100
C                                       Subsequent blocks.
 90      IS = 9
C                                       Begin loop thru block.
C                                       Check if block finished.
 100     IF (IS.GT.NWD) GO TO 60
C                                       Process record.
 110  IF ((GOTDAT) .AND. (DOALL.GT.0.0) .AND. (ITYP.NE.1) .AND.
     *   (ITYP.NE.7)) GO TO 900
      GO TO (500, 400, 300, 600, 200, 700, 800),  ITYP
C                                       RUN record.
 200  CONTINUE
         NUMANT = 0
C                                       Get info for CATBLK.
C                                       Get uv scaling
         SCUVN = 2.0D0**(2-BUFF1(IS+3))
C                                       Get observer
         CALL ZILI16 (2, BUFF1(IS), 1, ITEMP)
         CHTEMP = ' '
         CALL ZC8CL (4, 1, ITEMP, CHTEMP)
         CALL CHR2H (8, CHTEMP, 1, CATH(KHOBS))
C                                       Get MJAD and Obs. date.
         CALL ZR8P4 ('4IB8', BUFF1(IS+4), XMJAD)
         CALL ZR8P4 ('4IB8', BUFF1(IS+6), F8)
         XDAT = XMJAD + 2400000.5D0
C                                       Subtract 32768 from MJAD
         XMJAD = (XMJAD - 32768.0D0) * 65536.0D0
         MJAD = XMJAD
C                                       Convert Julian date to calender
         CALL GREG (XDAT, CHTEMP)
         CALL CHR2H (8, CHTEMP, 1, CATH(KHDOB))
C                                       VLB extension
         IF (BUFF1(IS-6).LE.32) GO TO 850
            DOFRQ = BUFF1(IS+10).EQ.2
            DOVLB = BUFF1(IS+10).GT.0
            XIAT = BUFF1(IS+11)
            XUT1 = 0.001 * BUFF1(IS+14)
            CALL ZR8P4 ('4IB8', BUFF1(IS+12), F8)
            GST0 = 360.0D0 * (2.0D0 **(-31)) * F8
            GO TO 850
C                                       ANTE records
 300     NUMANT = NUMANT + 1
C                                       Initialize antenna file
         IF (NUMANT.GT.1) GO TO 320
            CALL DUMPAN (MXXANT, FREQ, XIAT, XUT1, ANVER, DOVLB, IERR)
            IF ((ANVER.EQ.0) .OR. (GANDAT)) ANVER = ANVER + 1
            STFAC = .2997924562D0 * (2.0D0**(-12))
            IF (DOVLB) STFAC = STFAC * 1.D3
 320     CALL ZR8P4 ('4IB8', BUFF1(IS), BXSTA(NUMANT))
         BXSTA(NUMANT) = BXSTA(NUMANT) * STFAC
         CALL ZR8P4 ('4IB8', BUFF1(IS+2), BYSTA(NUMANT))
         BYSTA(NUMANT) = BYSTA(NUMANT) * STFAC
         CALL ZR8P4 ('4IB8', BUFF1(IS+4), BZSTA(NUMANT))
         BZSTA(NUMANT) = BZSTA(NUMANT) * STFAC
         CALL ZILI16 (4, BUFF1(IS+6), 1, ITEMP)
         NAMSTA(NUMANT) = '        '
         CALL ZC8CL (4, 1, ITEMP, NAMSTA(NUMANT)(1:4))
         GANDAT = F
         MXXANT = MAX (MXXANT, NUMANT)
         GO TO 850
C                                       SOUR records.
 400  CONTINUE
         NUMSOR = NUMSOR + 1.0
         IF (NUMSOR.GE.NITER+BITER) GO TO 900
C                                       Check if desired source.
         CALL ZILI16 (4, BUFF1(IS), 1, ITEMP)
         CALL ZC8CL (8, 1, ITEMP, SOURCE)
         CALL CHLTOU (8, SOURCE)
         EQUAL = .TRUE.
         IF (ISNAME) CALL CHWMAT (8, NAMEIN, SPAT, 1, SOURCE, EQUAL)
         WANTED = (EQUAL) .AND. ((JBAND.EQ.BUFF1(IS+26)) .OR.
     *      (JBAND.LT.0)) .AND. ((IQUAL.EQ.BUFF1(IS+4)) .OR.
     *      (IQUAL.LT.0))
         WANTED = (WANTED) .AND. (NUMSOR.GE.BITER) .AND.
     *      (NUMSOR.LT.NITER+BITER)
         IF ((GOTDAT) .AND. (NDOEOF)) GO TO 900
         LQUAL = BUFF1(IS+4)
         NUMANT = 0
         IF (.NOT.WANTED) GO TO 850
C                                       Found right source, get info
C                                       and create output file.
C                                       Get vis. scaling
         SCVIS = 2.0D0**(BUFF1(IS+5))
C                                       Get sort order.
         CALL ZILI16 (2, BUFF1(IS+8), 1, ITEMP)
         CALL ZC8CL (2, 1, ITEMP, CHTEMP)
         CALL CHR2H (2, CHTEMP, 1, CATH(KITYP))
C                                       Get RA
         CALL ZR8P4 ('4IB8', BUFF1(IS+18), F8)
         CATD(KDCRV+3) = 360.0 * (2.0D0**(-31)) * F8
         CATD(KDORA) = CATD(KDCRV+3)
C                                       Get Declination.
         CALL ZR8P4 ('4IB8', BUFF1(IS+20), F8)
         CATD(KDCRV+4) = 360.0 * (2.0D0**(-31)) * F8
         CATD(KDODE) = CATD(KDCRV+4)
C                                       Get frequency (Hz)
         CALL ZR8P4 ('4IB8', BUFF1(IS+22), CATD(KDCRV+2))
         CATD(KDCRV+2) = CATD(KDCRV+2) * 1.0D3
         FREQ = CATD(KDCRV+2) * 1.0D-9
C                                       Set uv scaling in wavelengths
         SCUV = FREQ*SCUVN
C                                       Get bandwidth (Hz)
         CALL ZR8P4 ('4IB8', BUFF1(IS+24), F8)
         CATR(KRCIC+2) = F8 * 1.0E3
C                                       Get source name
         CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Write message to user who is
C                                       waiting breathlessly to see if
C                                       his/her data is there.
         CALL H2CHR (8, 1, CATH(KHDOB), CHTEMP)
         CALL DATDAT (CHTEMP, OBSDAT)
         WRITE (MSGTXT,1400) SOURCE, LQUAL, FREQ, OBSDAT
         CALL MSGWRT (5)
         TIMADD = MAX ((ANVER-1) * 5.0, 0.0)
         BASADD = MAX ((ANVER-1) * 0.01, 0.0)
         IF ((STARTD) .AND. (GOTDAT)) GO TO 850
         IF (.NOT.STARTD) GO TO 410
            WRITE (MSGTXT,1401)
            CALL MSGWRT (6)
            CALL ZCLOSE (LUNO, INDO, IERR)
            IF (IERR.EQ.0) GO TO 405
               WRITE (MSGTXT,1402) IERR
               CALL MSGWRT (8)
               GO TO 990
 405        CALL MDESTR (FVOL(1), FCNO(1), CATBLK, BUFF2, I, IERR)
            IF (IERR.EQ.0) NCFILE = 0
            IF (IERR.EQ.0) GO TO 410
               WRITE (MSGTXT,1405) IERR
               CALL MSGWRT (6)
C                                       File name parms
 410     DEFNAM = NAMEIN
         IF (.NOT.WSNAME) DEFNAM = SOURCE
         IF (IQUAL.GE.0) WRITE (DEFNAM(9:12),1410) IQUAL
         DEFCLS = 'UVDATA'
         LOCNAM = NAMOUT
         LOCCLS = CLAOUT
         CATBLK(KIIMS) = 0
         IF (OUTSEQ.GE.1) CATBLK(KIIMS) = OUTSEQ
         CALL MAKOUT (DEFNAM, DEFCLS, 0, DEFCLS, LOCNAM, LOCCLS,
     *      CATBLK(KIIMS))
         CALL CHR2H (12, LOCNAM, KHIMNO, CATH(KHIMN))
         CALL CHR2H (6, LOCCLS, KHIMCO, CATH(KHIMC))
C                                       Get number of correlators.
         NCOR = BUFF1(IS+27)
         CATBLK(KINAX+1) = NCOR
C                                       VLB modification
         IF (.NOT.DOFRQ) GO TO 420
            CATBLK(KINAX+2) = 4
            CATBLK(KINAX+1) = 1
            NCOR = 4
C                                       Set output rec length words
 420     LENREC = NRPARM + CATBLK(KINAX) * CATBLK(KINAX+1) *
     *      CATBLK(KINAX+2) * CATBLK(KINAX+3) * MAX (1,CATBLK(KINAX+4))
C                                       Create catalogd output file.
         GOTDAT = F
         NUMVIS = 0
         IOCNT = 0
         CATBLK(KIGCN) = XVSMAX
         IDISO = DISOUT
         CALL UVCREA (IDISO, CCNO, BUFF2, IERR)
         IF (IERR.EQ.0) GO TO 425
            WRITE (MSGTXT,1420) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 990
C                                       Construct output file name.
 425     CALL ZPHFIL ('UV', IDISO, CCNO, 1, PNAME, IERR)
C                                       Mark output file in /CFILES/
         NCFILE = 1
         FVOL(1) = IDISO
         FCNO(1) = CCNO
         FRW(1) = 2
C                                       Initialize vis. count.
         XCNT = 0.0
C                                       Open output file.
         CALL ZOPEN (LUNO, INDO, IDISO, PNAME, T, T, T, IERR)
         IF (IERR.EQ.0) GO TO 430
            WRITE (MSGTXT,1425) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 990
C                                       Initialize output file.
 430     NUMVIS = 32760000
         NIOUT = LENBU
         SZBUFF = UVBFSL * 2
         CALL UVINIT ('WRIT', LUNO, INDO, NUMVIS, VO, LENREC, NIOUT,
     *      SZBUFF, BUFF2, BLKOF, KIND, IERR)
         IF (IERR.EQ.0) GO TO 440
            WRITE (MSGTXT,1430) IERR
            CALL MSGWRT (8)
            JERR = 8
            GO TO 990
 440     YREC = 0.0
         JERR = 8
C                                       Initialize vis count
         IOCNT = 0
         NUMVIS = 0
         STARTD = T
         GO TO 850
C
C                                       VIS records.
 500     NUMANT = 0
C                                       Check if correct source.
         IF (.NOT.WANTED) GO TO 850
C                                       Update vis. count and check
         XCNT = XCNT + 1.0
         IF (XCNT.LE.XVSMAX) GO TO 505
            WRITE (MSGTXT,1500)
            CALL MSGWRT (6)
            GO TO 900
C                                       Subtract MJAD from time.
 505     CALL ZR8P4 ('4IB8', BUFF1(IS+4), F8)
         F8 = F8 - MJAD
C                                       Copy to output buffer.
         INDEXX = KIND + IOCNT*LENREC
         IOCNT = IOCNT + 1
C                                       Random parameters - U
         BUFF2(INDEXX) = BUFF1(IS)*SCUV
C                                       V
         BUFF2(INDEXX+1) = BUFF1(IS+1)*SCUV
C                                       W
         BUFF2(INDEXX+2) = BUFF1(IS+2)*SCUV
C                                       Baseline
         BUFF2(INDEXX+3) = BUFF1(IS+3) + BASADD
C                                       Time (days)
         BUFF2(INDEXX+4) = F8*TIMFAC + TIMADD
C                                       Set further random parameters
C                                       here. NRPARM = # ran. parm.
C                                       Visibilities.
         DO 510 I = 1,NCOR
            IOFF = NRPARM+(I-1)*CATBLK(KINAX)
            BUFF2(INDEXX+IOFF) = BUFF1(IS+IOFF+1)*SCVIS
            BUFF2(INDEXX+IOFF+1) = BUFF1(IS+IOFF+2)*SCVIS
            BUFF2(INDEXX+IOFF+2) = BUFF1(IS+IOFF+3)
 510        CONTINUE
         GANDAT = T
         GOTDAT = T
C                                       See if output buffer is full.
         IF (IOCNT.LT.NIOUT) GO TO 850
C                                       Write output buffer.
            NIOUT = IOCNT
            IOCNT = 0
            YREC = YREC + 1.0
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT, KIND, IERR)
            IF (IERR.EQ.0) GO TO 520
               WRITE (MSGTXT,1510) IERR,YREC
               CALL MSGWRT (8)
               JERR = 8
               GO TO 990
C                                       Update vis. count.
 520        NUMVIS = NUMVIS + LENBU
            GO TO 850
C
C                                       DEF records.
 600     NUMANT = 0
         GO TO 850
C
C                                       END records
 700     NUMANT = 0
         GO TO 900
C
C                                       Records of unknown origin.
 800     GO TO 850
C
C                                       Get set for next record.
 850  IW(1) = BUFF1(IS-4)
      IW(2) = BUFF1(IS-3)
      ITYP = 7
      DO 860 I = 1,6
         EQUAL = (IW(1).EQ.ITYPES(1,I)) .AND. (IW(2).EQ.ITYPES(2,I))
         IF (EQUAL) ITYP = I
         IF (EQUAL) GO TO 870
 860     CONTINUE
C                                       Loop back for next record.
 870  IF (BUFF1(IS-6).GT.1) GO TO 875
         WRITE (MSGTXT,1870) BUFF1(IS-6)
         CALL MSGWRT (8)
         JERR = 8
         GO TO 990
 875  IS = IS + BUFF1(IS-6)/2
      GO TO 100
C                                       Finished; cleanup and go home.
C                                       Finish vis. count.
 900  NUMVIS = NUMVIS + IOCNT
C                                       Check if no data.
      IF (NUMVIS.NE.0) GO TO 905
         IF (DONONE) GO TO 990
            JERR = 5
            WRITE (MSGTXT,1900) NAMEIN, IQUAL, IBAND(JBAND+1)
            CALL MSGWRT (8)
            GO TO 990
C                                       Finish write and close files.
 905  YREC = YREC + 1.0
      NIOUT = -IOCNT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KIND, IERR)
      IF (IERR.EQ.0) GO TO 910
         WRITE (MSGTXT,1510) IERR, YREC
         CALL MSGWRT (8)
C                                       Write out last antenna file
C                                       Make sure one is written.
 910  IF (ANVER.LE.0) ANVER = 1
      CALL DUMPAN (MXXANT, FREQ, XIAT, XUT1, ANVER, DOVLB, IERR)
      DONONE = T
C                                       Shrink output file to correct
C                                       size
      CALL UCMPRS (NUMVIS, IDISO, CCNO, LUNO, CATBLK, IERR)
C                                       Write message about how many vis
      WRITE (MSGTXT,1910) NUMVIS
      CALL MSGWRT (5)
C                                       Close output file.
      CALL ZCLOSE (LUNO, INDO, IERR)
      IF (IERR.EQ.0) GO TO 930
         WRITE (MSGTXT,1911) IERR
         CALL MSGWRT (6)
 930  JERR = 0
C                                       Write history file
      CALL UVHIS
      NCFILE = 0
C                                       Reset parms and loop DOALL=T
      IF ((DOALL.LE.0.0) .OR. (ITYP.EQ.6)) GO TO 990
         STARTD = F
         GANDAT = F
         GOTDAT = F
         ANVER = 1
         TIMADD = 0.0
         BASADD = 0.0
         WANTED = F
         NUMVIS = 0
         IOCNT = 0
         IF (NUMSOR.LT.NITER+BITER) GO TO 110
 990  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, JERR)
      IF (IERR.EQ.0) IERR = JERR
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('VISIN: READ ERROR',I7,' TAPE BLOCK ',F10.0)
 1070 FORMAT ('VISIN: PHYSICAL RECORD LENGTH=',I8,' ERROR')
 1400 FORMAT ('VISIN: Found ',A8,' .',I4,F12.6,' GHz',1X,A)
 1401 FORMAT ('VISIN: PREVIOUS SOURCE WAS EMPTY: DESTROY IT')
 1402 FORMAT ('VISIN: UNABLE TO CLOSE PREVIOUS SOURCE, ERROR',I6)
 1405 FORMAT ('VISIN: UNABLE TO DELETE PREVIOUS SOURCE, ERROR',I6)
 1410 FORMAT (':',I3)
 1420 FORMAT ('VISIN: ERROR',I7,' CREATING CATALOGUED OUTPUT FILE')
 1425 FORMAT ('VISIN: ERROR',I7,' OPENING OUTPUT FILE')
 1430 FORMAT ('VISIN: ERROR',I7,' INIT OUTPUT FILE')
 1500 FORMAT ('WARNING: ESTIMATED NUMBER OF VIS RECORDS EXCEEDED:',
     *   ' DATA LOST?')
 1510 FORMAT ('VISIN: WRITE ERROR',I7,' BLOCK ',F10.0)
 1870 FORMAT ('VISIN: LOGICAL RECORD LENGTH',I8,' ERROR')
 1900 FORMAT ('VISIN: COULD NOT FIND ',A8,' . ',I3,2X,A1,' BAND')
 1910 FORMAT ('VISIN: COPIED ',I12,' VIS. RECORDS')
 1911 FORMAT ('VISIN: ERROR',I7,' CLOSING CATALOGUED UV FILE')
      END
      SUBROUTINE DUMPAN (MXXANT, FREQ, XIAT, XUT1, ANVER, DOVLB, IERR)
C-----------------------------------------------------------------------
C   DUMPAN creates an AN file and writes the AN common data to it.
C   Inputs: MXXANT  I     # antennas
C           FREQ    D     frequency (GHz)
C           XIAT    R     IAT - UTC
C           XUT1    R     UT1 - UTC
C           ANVER   I     desired version number (0 => none)
C           DOVLB   L     If .true. the data is from a VLBI array.
C   Output: IERR    I     error code of TABINI/TABAN
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXXANT, ANVER, IERR
      LOGICAL   DOVLB
      DOUBLE PRECISION FREQ, JD, GASTM, DEG2RD
      REAL      XIAT, XUT1
      INTEGER   LUNA,  IABUF(512), FINDA, I, JERR
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUNA /30/
C-----------------------------------------------------------------------
      IERR = 0
      DEG2RD = 3.141592653589793D0 / 180.0D0
      IF (ANVER.LE.0) GO TO 999
C                                       Setup for AN table initization
      NUMORB = 0
      NOPCAL = 2
      ANTNIF = 2
C                                       Position of the earth's pole
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
      UT1UTC = XUT1
      DATUTC = XIAT
      IF (ABS (XIAT) .GT. 0.1) THEN
         TIMSYS = 'IAT'
      ELSE
         TIMSYS = 'UTC'
         END IF
C                                       Array name and center
C                                       (rel to center of earth)
      IF (DOVLB) THEN
         ANAME = 'VLBI '
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
      ELSE
         ANAME = 'VLA '
C                                       old values
C                                       ARRAYC(1) = -1601162.D0
C                                       ARRAYC(2) =  -5042003.D0
C                                       ARRAYC(3) =  3554915.D0
C                                       new values 2-Jul-2001
         ARRAYC(1) = -1601185.365D0
         ARRAYC(2) =  -5041977.547D0
         ARRAYC(3) =  3554875.87D0
         END IF
C                                       Get GST0 and Earth rotation rate
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD)
      CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
      SAFREQ = FREQ * 1.0D9
      ANFQID = -1
      XYZHAN = ' '
      TFRAME = ' '
C                                       Create/init file
      CALL ANTINI ('WRIT', IABUF, FVOL(1), FCNO(1), ANVER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *   RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME,
     *   NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
      ANNAME = 'VLA: '
      STAXOF = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA = 0
      MNTSTA = 0
      POLAA = 0.0
      POLAB = 0.0
      CALL RFILL (3, 0.0, POLCA)
      CALL RFILL (3, 0.0, POLCB)
      POLTYA = 'R'
      POLTYB = 'L'
      DIAMAN = 25.0
      CALL RFILL (MAXIF, 0.0, FWHMAN)
      DO 10 I = 1,MXXANT
         STAXYZ(1) = BXSTA(I)
         STAXYZ(2) = BYSTA(I)
         STAXYZ(3) = BZSTA(I)
         NOSTA = I
         IF (.NOT.DOVLB) THEN
            ANNAME(5:8) = NAMSTA(I)(1:4)
         ELSE
            ANNAME = NAMSTA(I)
            END IF
         IANRNO = I
         CALL TABAN ('WRIT', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) GO TO 990
 10      CONTINUE
C                                       Close/update AN file.
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, IERR)
      GO TO 999
C                                       Error
 990  CALL ZCLOSE (LUNA, FINDA, JERR)
C
 999  RETURN
      END
      SUBROUTINE UVHIS
C-----------------------------------------------------------------------
C   UVHIS writes the history file for a uv data base: from EXPORT format
C   Inputs:
C      NAMEIN      C*8     Source name.
C      IQUAL       I       Source qualifier.
C      JBAND       I       Observing band (eg. 'L','C')
C      NUMVIS      I       Number of visibility points.
C      NAME
C      NAMOUT      C*12    Output file name
C      CLAOUT      C*6     Output file class.
C      SEQOUT      I       Output file sequence no.
C-----------------------------------------------------------------------
      CHARACTER CDUM*12, LNAM*12, LCLAS*6, HILINE*72, ATIME*8,
     *   HILI80*80, ADATE*12, CBAND(11)*2
      INTEGER   TIME(3), DATE(3), LUN, IERR
      REAL      DUM
      LOGICAL   T
      DOUBLE PRECISION XVIS
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
      DATA LUN, DUM /28, 0.0/
      DATA CBAND /'  ','P','L','C','U','K','X','S',3*' '/
C-----------------------------------------------------------------------
C                                       Create/open hist. file.
      CALL HICREA (LUN, FVOL(1), FCNO(1), CATBLK, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 900
C                                       Get current date/time.
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILI80,1010) TSKNAM, RLSNAM, ADATE, ATIME, NLUSER
      CALL HIAD80 (LUN, 1, HILI80, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Write file name,class,seq.
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAM)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLAS)
      CALL HENCOO (TSKNAM, LNAM, LCLAS, CATBLK(KIIMS), FVOL(1),
     *   LUN, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       If DISK file give name.
      IF (INFILE.NE.' ') THEN
         WRITE (HILINE,1013) TSKNAM, INFILE
         CALL HIADD (LUN, HILINE, SCRBUF, IERR)
         IF (IERR.NE.0) GO TO 20
         END IF
C                                       Source name/qualifier/band.
      WRITE (HILINE,1011) TSKNAM, NAMEIN, IQUAL, CBAND(JBAND+2)
      CALL HIADD (LUN, HILINE, SCRBUF, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Number of vis.
      XVIS = NUMVIS
      WRITE (HILINE,1012) TSKNAM, XVIS
      CALL HIADD (LUN, HILINE, SCRBUF, IERR)
 20   IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IERR
         CALL MSGWRT (6)
C                                       Close file
 30   CALL HICLOS (LUN, T, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 910
C                                       Create error: make sure header
C                                       got to catalog and
 900  CALL CATIO ('UPDT', FVOL(1), FCNO(1), CATBLK, 'REST', SCRBUF,
     *   IERR)
      NCFILE = NCFILE - 1
      IF (IERR.EQ.0) GO TO 910
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (6)
C                                       Clear write status.
 910  CALL CATDIR ('CSTA', FVOL(1), FCNO(1), CDUM, CDUM, DUM, CDUM, DUM,
     *   'CLWR', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVHIS: ERROR',I3,' CREATING HISTORY FILE')
 1010 FORMAT (A6,'Release=''',A7,' '' / Created at ',A,A,' by user',
     *   I5)
 1011 FORMAT (A6,' SOURCE=''',A8,''' QUAL=',I3,' BAND=''',A2,'''')
 1012 FORMAT (A6,' /Number of vis. points = ',F9.0)
 1013 FORMAT (A6,' INFILE=''',12A4,'''')
 1020 FORMAT ('UVHIS: ERROR',I3,' WRITING HISTORY FILE')
 1900 FORMAT ('ERROR',I7,' UPDATING CATALOG HEADER')
      END
      SUBROUTINE UVFHDR (ISLOT, IERR)
C-----------------------------------------------------------------------
C   UVFHDR reads the tape which must be open and positioned at beginning
C   of file and builds a catalog header and pointers from the tape
C   header records.  It then rewinds the tape in preparation for the
C   history subroutine.  It should work on any random-parameter FITS
C   tape, but is intended primarily for UV data.  UVFHDR also creates
C   the output file.
C   Output:
C      ISLOT  I     Catalog slot number for new UV file.
C      ERR   I        =0 => ok
C                     other => quit
C-----------------------------------------------------------------------
      CHARACTER DATOBS*11, CHTM8*8, DEFNAM*12, DEFCLS*6, LOCNAM*12,
     *   LOCCLS*6, FITBLK*2880, UVW(3)*4, UVW2(3)*4, ULVLWL(3)*4,
     *   UVWL2(3)*8, TELTYP*4, UVTP*2, STAT*4, PNAME*48, CHTEMP*8
      DOUBLE PRECISION XFREQ, JDAY, NEWD(128)
      REAL      ATEMP, NEWR(256)
      INTEGER   IWORK(256), ICARD, IERR, ISLOT, IREC, FPARMS, I, J, K,
     *   INC2, ISTVAL, ISTINC, ISTREF, IROUND, ISTNUM, NAXIS, IN, IS,
     *   IE, IAX, WTOFF, SCLOFF, LADR(4), LPIECE, IUSER, NEWBLK(256),
     *   IDLUN, IDFIND, ITRIM, ISIZE, LSIZE
      HOLLERITH NEWH(256)
      LOGICAL   END, F, T, TIMDON
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (NEWR, NEWBLK, NEWH, NEWD)
      EQUIVALENCE (IWORK, BUFF2)
      DATA UVW    /'UU  ','VV  ','WW  '/
      DATA UVW2   /'UU--','VV--','WW--'/
      DATA ULVLWL /'UU-L','VV-L','WW-L'/
      DATA UVWL2 /'UU-L-NCP', 'VV-L-NCP', 'WW-L-NCP'/
      DATA IDLUN /16/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Default scalling parm for wt.
      WTSCAL = 1.0D0
C                                       Init. Stokes pointers
C                                       Init. Stokes pointers
      JADR(1) = 1
      JADR(2) = 2
      JADR(3) = 3
      JADR(4) = 4
      CALL FILL (4, 0, NPV)
C                                       Initialize BLANK values flag
C                                       to false.
      ISBLNK = .FALSE.
C                                       Zero scaling values.
      DO 10 I = 1,20
         PSCAL(I) = 0.0D0
         POFF(I) = 0.0D0
         PTYPES(I) = ' '
 10      CONTINUE
C                                       open new version
      IF (IPIECE.GT.1) THEN
C                                       close old
         IF (DODISK) THEN
            CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
            I = ITRIM (INFILE)
            IF (IPIECE-1.GT.9) I = I - 1
            IF (IPIECE-1.GT.99) I = I - 1
            IF (IPIECE.LE.9) THEN
               WRITE (INFILE(I:),1010) IPIECE
            ELSE IF (IPIECE.LE.99) THEN
               WRITE (INFILE(I:),1011) IPIECE
            ELSE
               WRITE (INFILE(I:),1012) IPIECE
               END IF
            CALL CHR2H (48, INFILE, 1, FDVEC(7))
            CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1015) IERR
               CALL MSGWRT (8)
               GO TO 999
               END IF
            END IF
C                                       read 1st record
         CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       record 1 already read from tape.
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Decode required cards.
      CALL REQCD (FITBLK, ICEND, IERR)
      IF (IERR.NE.0) GO TO 999
      IF ((GROUP.EQ.1) .AND. (UVTABL)) GO TO 980
      IF ((GROUP.NE.1) .AND. (.NOT.UVTABL)) GO TO 980
      ICARD = ICEND + 1
C                                       Loop until END card found.
      DO 90 IREC = 1,100000000
C                                       Parse card, put value in hdr.
         CALL PARSCD (ICARD, FITBLK, END, IERR)
         IF (END) GO TO 100
         IF (IERR.GT.0) GO TO 999
         ICARD = ICARD + 1
 90      CONTINUE
C                                       Read more cards than we counted
C                                       on.
      WRITE (MSGTXT,1090)
      GO TO 990
C                                       End card found.
 100  CONTINUE
C                                       Reposition file
      CALL TAPIO ('BAKF', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check piece ID numbers
      LPIECE = IPIECE
      IF (UVTABL) THEN
         IF (NPV(1).GT.0) IPIECE = NPV(1)
         NPIECE = MIN (NPIECE, MAX (1, NPV(2)))
         IF (IPIECE.GT.NPIECE) THEN
            IERR = -1
            GO TO 999
            END IF
         IF (DPIECE.GT.0) CALL COPY (256, CATSAV, CATBLK)
         END IF
C                                       Check names.
C                                       Make axis increments non zero
C                                       to help out dumb programs.
      IN = KINAX
      IS = KRCIC
      IE = IS + CATBLK(KIDIM) - 1
      DO 101 IAX = IS,IE
         IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1)) CATR(IAX) = 1.0
         IN = IN + 1
 101     CONTINUE
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), DEFNAM)
      IF (DEFNAM.EQ.' ') DEFNAM = NAMEIN
      IF (DEFNAM.EQ.' ') CALL H2CHR (8, 1, CATH(KHOBJ), DEFNAM)
      IF (DEFNAM.EQ.' ') DEFNAM(1:8) = 'NO NAME'
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), DEFCLS)
      IF (DEFCLS.EQ.' ') DEFCLS = 'UVDATA'
      IF (OUTSEQ.GE.0) CATBLK(KIIMS) = OUTSEQ
      LOCNAM = NAMOUT
      LOCCLS = CLAOUT
      CALL MAKOUT (DEFNAM, DEFCLS, 0, DEFCLS, LOCNAM, LOCCLS,
     *   CATBLK(KIIMS))
      CALL CHR2H (12, LOCNAM, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, LOCCLS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'UV', KHPTYO, CATH(KHPTY))
C                                       Message to user
      CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
      CALL DATDAT (CHTM8, DATOBS)
      CALL H2CHR (8, 1, CATH(KHOBJ), CHTM8)
      WRITE (MSGTXT,1100) CHTM8, DATOBS
      IF ((CHTM8.NE.' ') .OR. (DATOBS.NE.'BAD DATE')) CALL MSGWRT (5)
C                                       Check PCOUNT
      IF (CATBLK(KIPCN).GT.20) GO TO 960
C                                       Make a number of changes needed
C                                       for our internal UV format.
      XFREQ = -1.0D0
      LCMPLX = F
      LSTOKE = F
      NAXIS = CATBLK(KIDIM)
      DO 150 I = 1,NAXIS
         J = 2 * (I - 1)  +  KHCTP
C                                       Complex axis
         CALL H2CHR (8, 1, CATH(J), CHTM8)
         IF (CHTM8.NE.'COMPLEX ') GO TO 110
            IF (I.EQ.1) GO TO 103
               WRITE (MSGTXT,1101) I
               GO TO 990
C                                       Check for non-standard Complex
 103        IF (CATBLK(KINAX+I-1).EQ.3) GO TO 150
               IF (CATBLK(KINAX+I-1).EQ.2) GO TO 105
               IF (UVTABL) GO TO 150
                  WRITE (MSGTXT,1103) CATBLK(KINAX+I-1)
                  GO TO 990
C                                       Check for need to convert
C                                       2 value complex to 3 value
C                                       complex numbers.
 105        CONTINUE
               LCMPLX = T
               CATBLK(KINAX+I-1) = 3
               GO TO 150
C                                       Check for stokes parameter that
C                                       we need to convert to RR, etc.
 110     IF ((CHTM8.NE.'STOKES  ') .OR. (UVTABL)) GO TO 130
            ATEMP = CATD(KDCRV+I-1) + (1-CATR(KRCRP+I-1)) *
     *         CATR(KRCIC+I-1)
            ISTVAL = IROUND (ATEMP)
            ISTINC = IROUND (CATR(KRCIC+I-1))
            ISTREF = IROUND (CATR(KRCRP+I-1))
            ISTNUM = CATBLK(KINAX+I-1)
            NSTACT = CATBLK(KINAX) * CATBLK(KINAX+1)
C                                       Non-standard Stokes
            IF ((ISTNUM.EQ.1) .AND. ((ABS(ISTVAL).EQ.1) .OR.
     *         (ISTVAL.EQ.-2))) GO TO 150
            IF ((ISTNUM.EQ.4) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.1))
     *         GO TO 150
            IF ((ISTNUM.EQ.3) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.1))
     *         GO TO 150
            IF (((ISTNUM.EQ.2) .OR. (ISTNUM.EQ.4)) .AND. (ISTVAL.EQ.-1)
     *         .AND. (ISTINC.EQ.-1)) GO TO 150
            IF (((ISTNUM.EQ.2) .OR. (ISTNUM.EQ.4)) .AND. (ISTVAL.EQ.-5)
     *         .AND. (ISTINC.EQ.-1)) GO TO 150
            IF ((ISTNUM.EQ.2) .AND. (ISTVAL.EQ.1) .AND. (ISTINC.EQ.3))
     *         GO TO 150
               IF ((I.EQ.2) .AND. (ISTNUM.EQ.4)) GO TO 115
 111              WRITE (MSGTXT,1111)
                  CALL MSGWRT (6)
                  GO TO 150
C                                       Stokes look up table
 115           DO 120 J = 1,ISTNUM
                  LADR(J) = ISTVAL + (J-ISTREF) * ISTINC
                  LADR(J) = ABS(LADR(J))
                  IF ((LADR(J).LT.1) .OR. (LADR(J).GT.4)) GO TO 111
 120              CONTINUE
               CALL COPY (4, LADR, JADR)
               LSTOKE = T
               CATD(KDCRV+I-1) = SIGN (1, ISTVAL)
               CATR(KRCIC+I-1) = CATD(KDCRV+I-1)
               CATR(KRCRP+I-1) = 1.0
               CATBLK(KINAX+I-1) = 4
               GO TO 150
C                                       Look for frequency.
 130     IF (CHTM8.EQ.'FREQ ') XFREQ = CATD(KDCRV+I-1)
 150     CONTINUE
C                                       Check for the use of 2 PVALS
C                                       values to represent 1 PTYPE
      NPARMS = CATBLK(KIPCN)
      IF ((PTYPES(NPARMS-1).EQ.'WEIGHT') .AND.
     *   (PTYPES(NPARMS).EQ.'SCALE')) NPARMS = NPARMS - 2
      FPARMS = 1
      ITAB(1) = 1
      CALL CHR2H (8, PTYPES(1), 1, CATH(KHPTP))
      DO 180 I = 2,NPARMS
         DO 160 K = 1,FPARMS
            INC2 = 2 * (K-1) + KHPTP
            CALL H2CHR (8, 1, CATH(INC2), CHTM8)
            IF (PTYPES(I).EQ.CHTM8) GO TO 170
 160        CONTINUE
C                                       No equal PTYPEn's this pass.
         FPARMS = FPARMS + 1
         INC2 = 2 * (FPARMS-1) + KHPTP
         IF (FPARMS.LE.KIPTPN) CALL CHR2H (8, PTYPES(I), 1, CATH(INC2))
         K = FPARMS
C                                       Set pointer
 170     ITAB(I) = K
 180     CONTINUE
      CATBLK(KIPCN) = FPARMS
C                                       Loop through parameters
C                                       looking for DATE change
C                                       to TIME1 and UU, VV, WW
C                                       to fix up.
      TIMDON = .FALSE.
      PBASE = 0
      DO 220 I = 1,NPARMS
         INC2 = 2 * (ITAB(I) - 1) + KHPTP
         IF (PTYPES(I).EQ.'BASELINE') PBASE = I
C                                       Convert DATE to TIME1.
         IF ((.NOT.TIMDON) .AND. (PTYPES(I).EQ.'DATE')) THEN
            CALL H2CHR (8, 1, CATH(KHDOB), CHTM8)
            CALL JULDAY (CHTM8, JDAY)
            POFF(I) = POFF(I) - JDAY
            CHTM8 = 'TIME1'
            CALL CHR2H (8, CHTM8, 1, CATH(INC2))
            TIMDON = .TRUE.
C                                       Multiply UU, VV, WW random
C                                       parameters by freq.
         ELSE IF (XFREQ.GT.0.0D0) THEN
            DO 190 J = 1,3
C                                       Found one.
               IF ((UVW(J).EQ.PTYPES(I)(:4)) .OR.
     *            (UVW2(J).EQ.PTYPES(I)(:4))) THEN
                  PSCAL(I) = PSCAL(I) * XFREQ
                  POFF(I) = POFF(I) * XFREQ
                  CALL H2CHR (4, 1, CATH(KHTEL), TELTYP)
                  IF (TELTYP.EQ.'ATCA') THEN
                     CALL CHR2H (8, UVWL2(J), 1, CATH(INC2))
                  ELSE
                     CALL CHR2H (4, ULVLWL(J), 1, CATH(INC2))
                     END IF
                  GO TO 220
                  END IF
 190           CONTINUE
            END IF
 220     CONTINUE
      CATBLK(KIIMU) = NLUSER
C                                       Compressed output data?
      IF (UVTABL) DOUVCM = .FALSE.
C                                       expand baseline if allowed
      IF (PBASE.GT.0) THEN
         I = FPARMS
         IF (DOUVCM) I = I + 2
         IF (I+2.LE.KIPTPN) THEN
            PTYPES(PBASE) = 'SUBARRAY'
            PBASE = ITAB(PBASE)
            INC2 = 2 * (PBASE - 1) + KHPTP
            CALL CHR2H (8, 'SUBARRAY', 1, CATH(INC2))
            CALL APPRPM ('ANTENNA1')
            PANT1 = CATBLK(KIPCN)
            CALL APPRPM ('ANTENNA2')
            PANT2 = CATBLK(KIPCN)
         ELSE
            PBASE = 0
            END IF
         END IF
      IF (DOUVCM) THEN
         MSGTXT = 'UV data will be written in compressed format'
         CALL MSGWRT (4)
         CATBLK(KINAX) = 1
C                                       Make sure that the random
C                                       parameter list contains one
C                                       WEIGHT parameter immediately
C                                       followed by a SCALE parameter:
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WTOFF = -1
            IERR = 0
            END IF
C                                       WTOFF is now the offset of the
C                                       WEIGHT parameter if it already
C                                       exists and -1 otherwise.
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), SCLOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            SCLOFF = -1
            IERR = 0
            END IF
C                                       SCLOFF is now the offset of the
C                                       SCALE parameter if it already
C                                       exists and -1 otherwise.
         IF ((WTOFF.GT.-1) .AND. (WTOFF.LT.NPARMS-1) .AND.
     *      (SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (WTOFF)
            WTOFF = -1
            END IF
         IF (WTOFF.EQ.-1) THEN
            CALL APPRPM ('WEIGHT  ')
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *         WTOFF, IERR)
C                                       AXEFND is not really necessary
C                                       here as APPRPM always adds a new
C                                       random parameter to the end of
C                                       the parameter list but leaves us
C                                       open for a more intelligent
C                                       APPRPM that re-uses deleted
C                                       parameters.
            END IF
         IF ((SCLOFF.NE.-1).AND.(SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (SCLOFF)
            SCLOFF = -1
            END IF
         IF (SCLOFF.EQ.-1) THEN
            CALL APPRPM ('SCALE   ')
            CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP),
     *         SCLOFF, IERR)
            END IF
         KLOCWT = WTOFF
         END IF
C                                       Does our file already exist
      ISLOT = 0
      IF (OUTSEQ.GT.0) THEN
         UVTP = 'UV'
         IUSER = NLUSER
         CALL CATDIR ('SRNH', DISOUT, ISLOT, LOCNAM, LOCCLS, OUTSEQ,
     *      UVTP, IUSER, STAT, SCRBUF, IERR)
         IF (IERR.EQ.5) THEN
            ISLOT = 0
            IERR = 0
            END IF
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR, 'CATDIR'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         END IF
C                                       File not found
      IF (ISLOT.LE.0) THEN
         INITVS = 0
C                                       But it is required
         IF (CONCAT.GT.0) THEN
            MSGTXT = 'OUTPUT CONCATANATION FILE NOT FOUND'
            CALL MSGWRT (8)
            IERR = -1
            GO TO 999
            END IF
C                                       Create the UV file.
         DPIECE = DPIECE + 1
         CALL UVCREA (DISOUT, ISLOT, IWORK, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       Tell common we have a UV file.
         NCFILE = 1
         FVOL(1) = DISOUT
         FCNO(1) = ISLOT
         FRW(1) = 2
         OUTSEQ = CATBLK(KIIMS)
C                                       File found
      ELSE
         IF (DPIECE.GT.0) CALL COPY (256, CATSAV, CATBLK)
         DPIECE = DPIECE + 1
C                                       But that is not allowed
         IF (CONCAT.EQ.-1) THEN
            MSGTXT = 'FILE ALREADY EXISTS'
            CALL MSGWRT (8)
            IERR = -1
            GO TO 999
            END IF
         CONCAT = 1
         CALL COPY (256, CATBLK, NEWBLK)
         CALL CATIO ('READ', DISOUT, ISLOT, CATBLK, 'WRIT', IWORK, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1220) IERR, 'CATBLK READ'
            CALL MSGWRT (8)
            GO TO 999
            END IF
         INITVS = CATBLK(KIGCN)
C                                       compare things
         IF (CATBLK(KIDIM).NE.NEWBLK(KIDIM)) THEN
            WRITE (MSGTXT,1230) CATBLK(KIDIM), NEWBLK(KIDIM),
     *          'AXIS COUNT'
            GO TO 990
            END IF
         IF ((CATBLK(KIPCN).NE.NEWBLK(KIPCN)) .AND.
     *      (CATBLK(KIPCN).NE.NEWBLK(KIPCN)-2) .AND.
     *      (CATBLK(KIPCN).NE.NEWBLK(KIPCN)+2)) THEN
            WRITE (MSGTXT,1230) CATBLK(KIPCN), NEWBLK(KIPCN),
     *          'PARAMETER COUNT'
            GO TO 990
            END IF
         K = MIN (CATBLK(KIPCN), NEWBLK(KIPCN))
         DO 230 I = 1,K
            J = KHPTP + 2 * I - 2
            CALL H2CHR (8, 1, CATH(J), CHTM8)
            CALL H2CHR (8, 1, NEWH(J), CHTEMP)
            IF (CHTEMP.NE.CHTM8) THEN
               WRITE (MSGTXT,1231) CHTM8, CHTEMP, 'RANDOM PARAMETER'
               GO TO 990
               END IF
 230        CONTINUE
         K = CATBLK(KIDIM)
         DO 240 I = 2,K
            J = KHCTP + 2 * I - 2
            CALL H2CHR (8, 1, CATH(J), CHTM8)
            CALL H2CHR (8, 1, NEWH(J), CHTEMP)
            IF (CHTEMP.NE.CHTM8) THEN
               WRITE (MSGTXT,1231) CHTM8, CHTEMP, 'AXIS PARAMETER'
               GO TO 990
               END IF
            IF (CATBLK(KINAX+I-1).NE.NEWBLK(KINAX+I-1)) THEN
               WRITE (MSGTXT,1230) CATBLK(KINAX+I-1), NEWBLK(KINAX+I-1),
     *            'AXIS DIMENSION'
               GO TO 990
               END IF
            IF (ABS(CATR(KRCRP+I-1)-NEWR(KRCRP+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATR(KRCRP+I-1), NEWR(KRCRP+I-1),
     *            'REFERENCE PIXEL'
               GO TO 990
               END IF
            IF (ABS(CATD(KDCRV+I-1)-NEWD(KDCRV+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATD(KDCRV+I-1), NEWD(KDCRV+I-1),
     *            'REFERENCE VALUE'
               GO TO 990
               END IF
            IF (ABS(CATR(KRCRT+I-1)-NEWR(KRCRT+I-1)).GT.0.01) THEN
               WRITE (MSGTXT,1232) CATR(KRCRT+I-1), NEWR(KRCRT+I-1),
     *            'ROTATION'
               GO TO 990
               END IF
 240        CONTINUE
C                                       expand file
         IF (.NOT.UVTABL) THEN
            CALL UVPGET (IERR)
            CALL ZPHFIL ('UV', DISOUT, ISLOT, 1, PNAME, IERR)
            CALL ZEXIST (DISOUT, PNAME, LSIZE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1220) IERR, 'FILE EXISTS CHECK'
               CALL MSGWRT (8)
               GO TO 999
               END IF
            CATBLK(KIGCN) = CATBLK(KIGCN) + NEWBLK(KIGCN)
            CALL UVSIZE (LREC, CATBLK(KIGCN), ISIZE)
            IF (ISIZE.GT.LSIZE) THEN
               CALL ZOPEN (IDLUN, IDFIND, DISOUT, PNAME, .TRUE., .TRUE.,
     *            .TRUE., IERR)
               IF (IERR.NE.0) GO TO 999
               I = ISIZE - LSIZE
               CALL ZEXPND (IDLUN, DISOUT, PNAME, I, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1220) IERR, 'EXPAND FILE'
                  CALL MSGWRT (8)
                  GO TO 999
                  END IF
               CALL ZCLOSE (IDLUN, IDFIND, IERR)
               IF (IERR.NE.0) GO TO 999
               END IF
            END IF
C                                       Tell common we have a UV file.
         NCFILE = 1
         FVOL(1) = DISOUT
         FCNO(1) = ISLOT
         FRW(1) = 1
         END IF
      GO TO 999
C                                       Too many random parameters.
 960  WRITE (MSGTXT,1960) CATBLK(KIPCN)
      GO TO 990
C                                       Not random parameter structure.
 980  WRITE (MSGTXT,1980)
      CALL MSGWRT (7)
      WRITE (MSGTXT,1981)
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT (I1)
 1011 FORMAT (I2)
 1012 FORMAT (I3)
 1015 FORMAT ('ERROR OPENING NEW DISK FILE:',I7)
 1090 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD')
 1100 FORMAT ('Found ',A8,' observed on ',A)
 1101 FORMAT ('FOUND COMPLEX IN AXIS',I3,'. WE ONLY HANDLE AXIS 1.')
 1103 FORMAT ('FOUND ILLEGAL COMPLEX AXIS WITH',I7,' POINTS')
 1111 FORMAT ('WARNING! STOKES VALUES NOT CONVERTED TO NORMAL FORMS')
 1220 FORMAT ('ERROR',I4,' ON ',A)
 1230 FORMAT ('MISMATCH',2I12,2X,A)
 1231 FORMAT ('MISMATCH ''',A,''' ''',A,'''',2X,A)
 1232 FORMAT ('MISMATCH',2(1PE12.4),2X,A)
 1960 FORMAT ('FOUND',I7,' RANDOM PARAMETERS.  MAX = 20')
 1980 FORMAT ('EXPECTED RANDOM PARAMETER STRUCTURE NOT FOUND.')
 1981 FORMAT ('UVLOD CANNOT DO IMAGES.  IT WORKS ON UV DATA ONLY.')
      END
      SUBROUTINE REQCD (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:  FITBLK    C*2880 a block of fit header data.
C   Outputs: ICARD        I   The number of the last card parsed.
C            IERR         I   0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C   COMMON /FITINF/ Sets GROUP to 1 if NAXIS1 is zero, else 0.
C-----------------------------------------------------------------------
      CHARACTER KL*80, SYMBOL*8, FITBLK*2880
      DOUBLE PRECISION    X
      INTEGER   ICARD, NPNT, IERR, ITYP, NAXIS, ITABNO, IVAL, I, IAX,
     *   IKEYWD
      LOGICAL   ISHIST, END
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL, SYMBOL,
     *   ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.0) IVAL = X + 0.1
      IF (X.LT.0.0) IVAL = X - 0.1
      IBPP = IVAL
      IF ((IVAL.NE.8) .AND. (IVAL.NE.16) .AND. (IVAL.NE.32) .AND.
     *   (IVAL.NE.-32) .AND. (IVAL.NE.-64)) GO TO 950
      IF (IVAL.EQ.-64) THEN
         MSGTXT = 'WARNING: 64-bit input stored in 32 bits inside AIPS'
         CALL MSGWRT (6)
         END IF
C                                       Check NAXIS
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      NAXIS = X + .01
      USED(IKEYWD) = USED(IKEYWD) + 1
C                                       Check NAXIS1 for group format.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL, SYMBOL,
     *   ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 980
      USED(IKEYWD) = USED(IKEYWD) + 1
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IVAL = X + .01
      IAX = KINAX
C                                       Set values for group structure.
      GROUP = 1
      CATBLK(KIDIM) = NAXIS - 1
C                                       Reset values for non group.
      IF (IVAL.EQ.0) GO TO 20
         GROUP = 0
         CATBLK(KIDIM) = NAXIS
         CATBLK(IAX) = IVAL
         IAX = IAX + 1
C                                       Check for invalid no. of axis
C                                       for our header.
 20   IF ((NAXIS.LT.2) .OR. (CATBLK(KIDIM).GT.7)) GO TO 960
C                                       Check NAXISm
      DO 30 I = 2,NAXIS
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *      SYMBOL, ITABNO, ISHIST, END, IERR)
         IF (IERR.NE.0) GO TO 970
         USED(IKEYWD) = USED(IKEYWD) + 1
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         CATBLK(IAX) = X + .01
         IAX = IAX + 1
 30      CONTINUE
C                                       No longer use EXTEND card
      STDEXT = .TRUE.
      GO TO 999
C                                       Not SIMPLE FITS tape.
 940  WRITE (MSGTXT,1940)
      GO TO 980
C                                       Invalid bits per pixel value.
 950  WRITE (MSGTXT,1950) IVAL
      GO TO 980
C                                       Invalid number of axis.
 960  WRITE (MSGTXT,1960) NAXIS
      GO TO 980
C                                       Expected keyword not found.
 970  WRITE (MSGTXT,1970) CWORD(IKEYWD), SYMBOL
      GO TO 980
 975  MSGTXT = 'REQCD: NUMBER ERROR ON ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE PARSCD (ICARD, FITBLK, END, IERR)
C-----------------------------------------------------------------------
C   PARSCD (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error.
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      CHARACTER KL*80, SYMBOL*8, STR*80, FITBLK*2880, CHTEMP*24
      DOUBLE PRECISION X
      REAL      VAL
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, JERR, IERR, ICARD, NPNT,
     *   KT, IL, IVAL, NCHAR, NBYT, NN, NNSTR, IT, NPNTS
      LOGICAL   ISHIST, END, LHIST, FIRST
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFUV.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      FIRST = .TRUE.
      NN = NKT + NCT
      NNSTR = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
 10   CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, JERR)
      IF (END) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
      IF (ISHIST) THEN
         IF (XERROR.GT.1.5) GO TO 999
         IF (JERR.EQ.1) GO TO 10
         IF ((USED(TABNO).GT.0) .AND. (JERR.EQ.0)) GO TO 10
         END IF
      IF ((JERR.EQ.1) .AND. ((SYMBOL(:2).EQ.'CD') .OR.
     *   (SYMBOL(:2).EQ.'PC') .OR. (SYMBOL(:2).EQ.'PV')))
     *   CALL PCCARD (GROUP, KL, PCMATX, CDMATX, PVMATX)
      IF (JERR.NE.0) GO TO 999
      IF (.NOT.ISHIST) USED(TABNO) = USED(TABNO) + 1
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
C                                       Readjust axis pointers for grp
      IF ((GROUP.EQ.1) .AND. (TABNO.GE.21) .AND. (TABNO.LE.60))
     *   IPOFF = IPOFF - 1
C                                       Type value of keyword
C                                       1=LOGICAL
C                                       2=NUMBER
C                                       3=STRING
      KT = ATYPE(TABNO)
C                                       Logical value
      NPNTS = NPNT
      IF (KT.EQ.1) THEN
         CALL GETLG (KL, 80, NPNT, IL)
C                                       Illegal logical value.
         IF (IL.LT.0) THEN
            WRITE (MSGTXT,1100) SYMBOL
            GO TO 990
            END IF
C                                       Check for logical value
C                                       special cases.
         IF (AWORD(TABNO).EQ.'GROUPS') THEN
C                                       Structure indicated by NAXIS
C                                       and GROUP don't match.
            IF (GROUP.NE.IL) THEN
               IERR = 1
               WRITE (MSGTXT,1110)
               GO TO 990
               END IF
C                                       Handle normal logical cases.
         ELSE
            CATBLK(PNTR+IPOFF) = IL
            END IF
C                                       Number
      ELSE IF (KT.EQ.2) THEN
         CALL GETNUM (KL, 80, NPNT, X)
C                                       special parse for EQUINOX
         IF (X.EQ.DBLANK) THEN
            IF ((AWORD(TABNO).EQ.'EQUINOX') .OR.
     *         (AWORD(TABNO).EQ.'EPOCH')) THEN
               NPNT = NPNTS
               CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
               IF (INDEX(STR,'1950').GT.0) THEN
                  X = 1950.0D0
               ELSE IF (INDEX(STR,'2000').GT.0) THEN
                  X = 2000.0D0
                  END IF
               END IF
            END IF
         IF (X.EQ.DBLANK) THEN
            MSGTXT = 'PARSCD NUMBER ERROR ON ' // SYMBOL
            CALL MSGWRT (7)
            X = 0.0D0
            END IF
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            IF (X.EQ.-2147483648.0D0) THEN
               IBLANK = -2147483647 - 1
            ELSE
               IBLANK = X
               END IF
            ISBLNK = .TRUE.
C                                       Scaling factors and offsets for
C                                       random parms (used in FITDAT).
         ELSE IF (AWORD(TABNO)(:5).EQ.'PSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TSCAL') THEN
            CALL GETI (AWORD(TABNO), IT)
            PSCAL(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'PZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
         ELSE IF (AWORD(TABNO)(:5).EQ.'TZERO') THEN
            CALL GETI (AWORD(TABNO), IT)
            POFF(IT) = X
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       4-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IVAL = X + SIGN (0.5D0, X)
            IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
            IF (PNTR.GT.0) CATBLK(PNTR+IPOFF) = IVAL
            IF (AWORD(TABNO).EQ.'IPIECE  ') THEN
               NPV(1) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'NPIECE  ') THEN
               NPV(2) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'FIRSTVIS') THEN
               NPV(3) = IVAL
            ELSE IF (AWORD(TABNO).EQ.'LASTVIS ') THEN
               NPV(4) = IVAL
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
            IF (AWORD(TABNO).EQ.'WTSCAL') WTSCAL = X
C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) CATD(PNTR+IPOFF) = X
            IF (AWORD(TABNO).EQ.'BSCALE') BSC = X
            IF (AWORD(TABNO).EQ.'BZERO') BZE = X
            END IF
C                                       String
      ELSE IF (KT.EQ.3) THEN
         CALL GETSTR (KL, 80, 68, NPNT, STR, NCHAR)
C                                       Dates are special
         IF (AWORD(TABNO)(:4).EQ.'DATE') THEN
            CALL DATFST ('F2L', STR)
            NCHAR = 8
            END IF
         NCHAR = MIN (NBYT, NCHAR)
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CHTEMP = ' '
            CALL CHR2H (NBYT, CHTEMP, IPOFF, CATH(PNTR))
            CALL CHR2H (NCHAR, STR(1:NCHAR), IPOFF, CATH(PNTR))
         ELSE
            IPOFF = ((NBYT+3)/4) * IPOFF
C                                       Start string on integer boundary
            IF (AWORD(TABNO)(:7).EQ.'SORTORD') THEN
               CATH(PNTR+IPOFF) = HBLANK
               CALL CHR2H (NCHAR, STR(1:NCHAR), 1, CATH(PNTR+IPOFF))
C                                       Random parameter type
            ELSE IF (AWORD(TABNO)(:5).EQ.'PTYPE') THEN
               CALL GETI (AWORD(TABNO), IT)
               PTYPES(IT) = STR(1:NCHAR)
C                                       Start string on real boundary.
            ELSE
               CHTEMP = ' '
               CALL CHR2H (NBYT, CHTEMP, 1, CATH(PNTR+IPOFF))
               CALL CHR2H (NCHAR, STR(1:NCHAR), 1, CATH(PNTR+IPOFF))
               END IF
            END IF
         END IF
C                                       Loop on History cards
      IF (ISHIST) GO TO 10
      GO TO 999
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT (A8,' LOGICAL VARIABLE HAS ILLEGAL VALUE')
 1110 FORMAT ('NAXIS AND GROUP STRUCTURE DO NOT MATCH')
      END
      SUBROUTINE UVFDAT (ISLOT, IERR)
C-----------------------------------------------------------------------
C   UVFDAT reads the tape (which must be open and positioned at begin.
C   of the binary data) and reorders and floats the binary data,
C   writing it to an AIPS random parameter catalogd file.  It leaves
C   the tape at the start of the next file.  It should work on any
C   random-parameter FITS tape, but is intended primarily for UV data.
C   Input:
C      ISLOT   I   catalog slot number of UV file.
C   Output:
C      IERR    I   0 => ok
C                  other => quit
C-----------------------------------------------------------------------
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER UVNAME*48
      REAL      IOBLK(UVBFSL), RECR(UVBFSL), RECBYR(UVBFSL), XF,
     *   TBUFF(UVBFSL), WTS(10),  FITBR(720), BASEL
      DOUBLE PRECISION FITBD(360), RECBYD(UVBFSL/2)
      INTEGER   NVISOF, DVIS, VISMAX, NWDLF, FITBIN(1440), IERR, IDCTR,
     *   IVCTR, IVMAX, IDFIND, IDLUN, INDEXX, NCORR, MRPARM, NTOTAL,
     *   NAXIS, NAXVAL, ISTCNT, I, ISIZE, ISLOT, IWTCTR, NEXT, LOOP,
     *   RECBYT(UVBFSL), DATTYP, LIMIT, NW, IP, IP2, OUTP, NUMCPX,
     *   NCMPLX, VISPNT(MAXCIF), NCOPY, NMOV, POINT, NUMDO, NSTOKE,
     *   STKNUM, JNCS, STKCNT, NBLKOF, ZTYPE, ISUB, IANT1, IANT2
      LOGICAL   MAP, EXCL, WAIT, GOOD
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (RECBYT, RECBYR, RECBYD)
      EQUIVALENCE (BUFF1, FITBIN, FITBR, FITBD),     (BUFF2, IOBLK)
      EQUIVALENCE (MAP, EXCL, WAIT)
      DATA MAP /.TRUE./
      DATA NVISOF, NBLKOF, IDLUN /0, 1, 16/
C-----------------------------------------------------------------------
C                                       Crunch CATBLK
      MSGSUP = 32000
      CALL UVPGET (IERR)
      MSGSUP = 0
      CALL FILL (6, 0, NZERO)
C                                       Open UV file.
      CALL ZPHFIL ('UV', DISOUT, ISLOT, 1, UVNAME, IERR)
      CALL ZOPEN (IDLUN, IDFIND, DISOUT, UVNAME, MAP, EXCL, WAIT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine number of groups to
C                                       process.
      VISMAX = CATBLK(KIGCN)
C                                       Data type
      DATTYP = ABS (IBPP) / 8
      NW = 2880 / DATTYP
      NUMCPX = CATBLK(KINAX)
      NCMPLX = 3
      IF (LCMPLX) NCMPLX = 2
C                                       Determine number of values in
C                                       a group.
      NAXIS = CATBLK(KIDIM)
      NAXVAL = NCMPLX
      DO 10 I = 2,NAXIS
         NAXVAL = NAXVAL * CATBLK(KINAX+I-1)
 10      CONTINUE
      NCORR = NAXVAL / NCMPLX
      MRPARM = CATBLK(KIPCN)
C                                      Total vals/visrec on tape.
      NTOTAL = NPARMS + NAXVAL
C                                       test our limitations
      IF ((NTOTAL.LE.UVBFSL) .AND. (LREC.LE.UVBFSL) .AND.
     *   (NCORR.LE.MAXCIF)) GO TO 15
         WRITE (MSGTXT,1010) NTOTAL, LREC
         IERR = 2
         GO TO 995
C                                       Set visibility pointers
C                                       Put Stokes' in proper order
 15   NSTOKE = 1
      IF (JLOCS.GT.0) NSTOKE = CATBLK(KINAX+JLOCS)
      IF (JLOCS.LT.0) INCS = NUMCPX
      JNCS = INCS / NUMCPX
      LIMIT = (3 * (NAXVAL / NCMPLX)) / NUMCPX
      DO 20 LOOP = 1,LIMIT
         STKCNT = (LOOP-1) / JNCS
         STKNUM = MOD (STKCNT, NSTOKE) + 1
         INDEXX = (LOOP-1) + (JADR(STKNUM) - STKNUM) * JNCS
         VISPNT(LOOP) = CATBLK(KIPCN) + INDEXX * NUMCPX
         IF (DOUVCM) VISPNT(LOOP) = CATBLK(KIPCN) + INDEXX * 3
 20      CONTINUE
C                                       Initialize uv writing system.
      ISIZE = 2 * UVBFSL
      IVMAX = 0
      CALL UVINIT ('WRIT', IDLUN, IDFIND, CATBLK(KIGCN), NVISOF, LREC,
     *   IVMAX, ISIZE, IOBLK, NBLKOF, IDCTR, IERR)
      IF (IERR.NE.0) GO TO 999
      IVCTR = 0
      IWTCTR = 1
      ISTCNT = 0
      DVIS = 0
C                                      Read first record
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 970
      NEXT = 1
C                                       Convert data types
      IF (IBPP.EQ.8) CALL ZI8IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.16) CALL ZI16IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.32) CALL ZI32IL (NW, 1, TAPBUF(TBIND), FITBIN)
      IF (IBPP.EQ.-32) CALL ZR32RL (NW, 1, TAPBUF(TBIND), FITBR)
      IF (IBPP.EQ.-64) CALL ZR64RL (NW, 1, TAPBUF(TBIND), FITBD)
C                                       Loop until we exhaust data.
 40   CONTINUE
         DVIS = DVIS + 1
C                                       Get next record from tape
         POINT = 1
         NMOV = NTOTAL
C                                       Determine number of words in
C                                       current record.
 50         NWDLF = NW + 1 - NEXT
            NCOPY = NMOV
            IF (NWDLF.LT.NCOPY) NCOPY = NWDLF
            NUMDO = NCOPY
C                                       integers
            IF (IBPP.GT.0) THEN
               CALL COPY (NUMDO, FITBIN(NEXT), RECBYT(POINT))
C                                       32-bit floating
            ELSE IF (IBPP.EQ.-32) THEN
               CALL RCOPY (NUMDO, FITBR(NEXT), RECBYR(POINT))
C                                       64-bit floating
            ELSE
               CALL DCOPY (NUMDO, FITBD(NEXT), RECBYD(POINT))
               END IF
C                                       Are we done with I/O for now?
            NEXT = NEXT + NCOPY
C                                       More to do
            IF (NCOPY.GE.NMOV) GO TO 100
               POINT = POINT + NCOPY
               NMOV = NMOV - NCOPY
C                                       Read next record
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
               IF (IERR.NE.0) GO TO 970
C                                       Convert data types
               IF (IBPP.EQ.8) CALL ZI8IL (NW, 1, TAPBUF(TBIND), FITBIN)
               IF (IBPP.EQ.16) CALL ZI16IL (NW, 1, TAPBUF(TBIND),
     *            FITBIN)
               IF (IBPP.EQ.32) CALL ZI32IL (NW, 1, TAPBUF(TBIND),
     *            FITBIN)
               IF (IBPP.EQ.-32) CALL ZR32RL (NW, 1, TAPBUF(TBIND),
     *            FITBR)
               IF (IBPP.EQ.-64) CALL ZR64RL (NW, 1, TAPBUF(TBIND),
     *            FITBD)
               NEXT = 1
               GO TO 50
C                                       Scale and offset to R:
C                                       Integer inputs
 100     IF (IBPP.GT.0) THEN
C                                       Random parms
            DO 120 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYT(LOOP) + POFF(LOOP)
 120           CONTINUE
C                                       Regular data
            LIMIT = NPARMS + 1
            IF (.NOT.ISBLNK) THEN
               DO 125 LOOP = LIMIT,NTOTAL
                  RECR(LOOP) = BSC * RECBYT(LOOP) + BZE
 125              CONTINUE
               GO TO 200
C                                       Possibly blanked
            ELSE
               DO 130 LOOP = LIMIT,NTOTAL
                  IF (RECBYT(LOOP).EQ.IBLANK) THEN
                     RECR(LOOP) = FBLANK
                  ELSE
                     RECR(LOOP) = BSC * RECBYT(LOOP) + BZE
                     END IF
 130              CONTINUE
               END IF
C                                       32 bit floating input
         ELSE IF (IBPP.EQ.-32) THEN
C                                       Random parameters
            DO 140 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYR(LOOP) + POFF(LOOP)
 140           CONTINUE
C                                       Possibly blanked
C                                       Regular data
            LIMIT = NPARMS + 1
            DO 145 LOOP = LIMIT,NTOTAL
               IF (RECBYR(LOOP).EQ.FBLANK) THEN
                  RECR(LOOP) = FBLANK
               ELSE
                  RECR(LOOP) = BSC * RECBYR(LOOP) + BZE
                  END IF
 145           CONTINUE
C                                       64 bit floating input
         ELSE
C                                       Random parameters
            DO 150 LOOP = 1,NPARMS
               RECR(LOOP) = PSCAL(LOOP) * RECBYD(LOOP) + POFF(LOOP)
 150           CONTINUE
C                                       Possibly blanked
C                                       Regular data
            LIMIT = NPARMS + 1
            DO 155 LOOP = LIMIT,NTOTAL
               IF (RECBYD(LOOP).EQ.DBLANK) THEN
                  RECR(LOOP) = FBLANK
               ELSE
                  RECR(LOOP) = BSC * RECBYD(LOOP) + BZE
                  END IF
 155           CONTINUE
            END IF
C                                       Other data type go here:
C                                       Sum to output record
 200     CALL RFILL (LREC, 0, IOBLK(IDCTR))
         CALL RFILL (NUMCPX, 1, WTS)
         WTS(3) = WTSCAL
C                                       Random  parms
         DO 210 LOOP = 1,NPARMS
            IP = IDCTR + ITAB(LOOP) - 1
            IOBLK(IP) = IOBLK(IP) + RECR(LOOP)
 210        CONTINUE
         IF ((PBASE.GT.0) .OR. (ILOCB.GE.0)) THEN
            IF (PBASE.GT.0) THEN
               IP = IDCTR-1+PBASE
            ELSE
               IP = IDCTR+ILOCB
               END IF
            ZTYPE = IOBLK(IP) + 0.01
            IF ((ZTYPE/257)*257.EQ.ZTYPE) THEN
               ZTYPE = 2
            ELSE
               ZTYPE = 1
               END IF
         ELSE IF ((ILOCA1.GE.0) .AND. (ILOCA2.GE.0)) THEN
            IF (IOBLK(IDCTR+ILOCA1).EQ.IOBLK(IDCTR+ILOCA2)) THEN
               ZTYPE = 2
            ELSE
               ZTYPE = 1
               END IF
            END IF
C                                       baseline -> subarray, antenna1/2
         IF (PBASE.GT.0) THEN
            BASEL = IOBLK(IDCTR-1+PBASE)
            IANT1 = BASEL/256
            BASEL = BASEL - 256*IANT1
            IANT2 = BASEL
            ISUB = (BASEL - IANT2) * 100.0 + 1.05
            IOBLK(IDCTR-1+PBASE) = ISUB
            IOBLK(IDCTR-1+PANT1) = IANT1
            IOBLK(IDCTR-1+PANT2) = IANT2
            END IF
C                                       Regular data
C                                       Normal Complex and unblanked
         LIMIT = NPARMS + 1
         IF ((LCMPLX) .OR. (ISBLNK)) GO TO 240
C                                       Compress?
            IP = 1
            IF (DOUVCM) THEN
               DO 220 LOOP = LIMIT,NTOTAL,3
                  IP2 = VISPNT(IP) - MRPARM
                  IF ((RECR(LOOP).EQ.FBLANK) .OR.
     *               (RECR(LOOP+1).EQ.FBLANK) .OR.
     *               (RECR(LOOP+2).EQ.FBLANK)) THEN
                     CALL RFILL (3, 0.0, TBUFF(1+IP2))
                  ELSE
                     TBUFF(1+IP2) = RECR(LOOP)
                     TBUFF(2+IP2) = RECR(LOOP+1)
                     TBUFF(3+IP2) = RECR(LOOP+2) * WTSCAL
                     END IF
                  IP = IP + 1
 220              CONTINUE
               CALL UVZERO (CATBLK, TBUFF, NZERO(1,ZTYPE), GOOD)
               CALL ZUVPAK (NCORR, TBUFF, IOBLK(IDCTR+KLOCWT),
     *            IOBLK(IDCTR+MRPARM))
            ELSE
               DO 230 LOOP = LIMIT,NTOTAL,3
                  IP2 = VISPNT(IP)
                  IF ((RECR(LOOP).EQ.FBLANK) .OR.
     *               (RECR(LOOP+1).EQ.FBLANK) .OR.
     *               (RECR(LOOP+2).EQ.FBLANK)) THEN
                     CALL RFILL (3, 0.0, IOBLK(IDCTR+IP2))
                  ELSE
                     IOBLK(IDCTR+IP2) = RECR(LOOP)
                     IOBLK(IDCTR+IP2+1) = RECR(LOOP+1)
                     IOBLK(IDCTR+IP2+2) = RECR(LOOP+2) * WTSCAL
                     END IF
                  IP = IP + 1
 230              CONTINUE
               CALL UVZERO (CATBLK, IOBLK(IDCTR+CATBLK(KIPCN)),
     *            NZERO(1,ZTYPE), GOOD)
               END IF
            GO TO 290
C                                       Blanked and/or 2 complex axis
C                                       data.
 240     CONTINUE
            IP = 0
C                                       Compress?
            IF (DOUVCM) THEN
               DO 250 LOOP = LIMIT,NTOTAL,NCMPLX
                  IP = IP + 1
                  OUTP = VISPNT(IP) - MRPARM + 1
C                                       Check if blanked
                  IF ((RECR(LOOP).NE.FBLANK) .AND.
     *               (RECR(LOOP+1).NE.FBLANK)) THEN
                     TBUFF(OUTP) = RECR(LOOP)
                     TBUFF(OUTP+1) = RECR(LOOP+1)
                     IF (LCMPLX) THEN
                        TBUFF(OUTP+2) = 1.0
                     ELSE
                        TBUFF(OUTP+2) = RECR(LOOP+2) * WTSCAL
                        END IF
                  ELSE
C                                       Blanked
                     TBUFF(OUTP) = 0.0
                     TBUFF(OUTP+1) = 0.0
                     TBUFF(OUTP+2) = 0.0
                     END IF
 250              CONTINUE
               CALL UVZERO (CATBLK, TBUFF, NZERO(1,ZTYPE), GOOD)
               CALL ZUVPAK (NCORR, TBUFF, IOBLK(IDCTR+KLOCWT),
     *            IOBLK(IDCTR+MRPARM))
            ELSE
C                                       Normal output data
               DO 260 LOOP = LIMIT,NTOTAL,NCMPLX
                  IP = IP + 1
                  OUTP = VISPNT(IP) + IDCTR
C                                       Check if blanked
                  IF ((RECR(LOOP).NE.FBLANK) .AND.
     *               (RECR(LOOP+1).NE.FBLANK)) THEN
                     IOBLK(OUTP) = RECR(LOOP)
                     IOBLK(OUTP+1) = RECR(LOOP+1)
                     IF (LCMPLX) THEN
                        IOBLK(OUTP+2) = 1.0
                     ELSE
                        IOBLK(OUTP+2) = RECR(LOOP+2) * WTSCAL
                        END IF
                  ELSE
C                                       Blanked
                     IOBLK(OUTP) = 0.0
                     IOBLK(OUTP+1) = 0.0
                     IOBLK(OUTP+2) = 0.0
                     END IF
 260              CONTINUE
               CALL UVZERO (CATBLK, IOBLK(IDCTR+CATBLK(KIPCN)),
     *            NZERO(1,ZTYPE), GOOD)
               END IF
C                                       Write
 290     IF ((DOKEEP.GT.0.0) .OR. (GOOD)) THEN
            IVCTR = IVCTR + 1
            IDCTR = IDCTR + LREC
C                                       Write this full buffer.
            IF (IVCTR.GE.IVMAX) THEN
               CALL UVDISK ('WRIT', IDLUN, IDFIND, IOBLK, IVMAX, IDCTR,
     *            IERR)
               IF (IERR.NE.0) GO TO 980
               IVCTR = 0
               END IF
         ELSE
            NSKIP = NSKIP + 1
            END IF
C                                       Test to see if we continue main
C                                       loop.
         IF (DVIS.LT.VISMAX) GO TO 40
C                                       Finish up any pending disk I/O.
      IVCTR = -IVCTR
      CALL UVDISK ('FLSH', IDLUN, IDFIND, IOBLK, IVCTR, IDCTR, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       not all written
      IF (NSKIP.GT.0) THEN
         VISMAX = VISMAX - NSKIP
         CALL UCMPRS (VISMAX, DISOUT, ISLOT, IDLUN, CATBLK, IERR)
         IERR = 0
         END IF
C                                       messages
      DO 310 ZTYPE = 1,2
         IF (ZTYPE.EQ.1) THEN
            MSGTXT = 'UVFDAT: counts for cross-correlations'
         ELSE
            MSGTXT = 'UVFDAT: counts for auto-correlations'
            END IF
         IF ((NZERO(1,ZTYPE).GT.0) .OR. (NZERO(2,ZTYPE).GT.0))
     *      CALL MSGWRT (4)
         IF (NZERO(1,ZTYPE).GT.0) THEN
            WRITE (MSGTXT,1300) NZERO(1,ZTYPE)
            CALL MSGWRT (4)
            IF (NZERO(3,ZTYPE).GT.0) THEN
               XF = 100.0 * NZERO(1,ZTYPE)
               XF = XF / NZERO(3,ZTYPE)
               WRITE (MSGTXT,1301) XF
               CALL MSGWRT (4)
               END IF
            END IF
         IF (NZERO(2,ZTYPE).GT.0) THEN
            WRITE (MSGTXT,1302) NZERO(2,ZTYPE)
            CALL MSGWRT (4)
            IF (NZERO(3,ZTYPE).GT.0) THEN
               XF = 100.0 * NZERO(2,ZTYPE)
               XF = XF / NZERO(3,ZTYPE)
               WRITE (MSGTXT,1303) XF
               CALL MSGWRT (4)
               END IF
            END IF
 310     CONTINUE
      IF (NSKIP.GT.0) THEN
         WRITE (MSGTXT,1310) NSKIP
         CALL MSGWRT (4)
         END IF
      GO TO 999
C                                       Problem reading tape.
 970  WRITE (MSGTXT,1970) IERR
      GO TO 995
C                                       Problem writing data.
 980  WRITE (MSGTXT,1980) IERR
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('UVFDAT: BUFFERS TOO SMALL FOR THESE DATA; NEED',2I8)
 1300 FORMAT ('UVFDAT: flagged',I9,' valid spectra samples which',
     *   ' were pure 0')
 1301 FORMAT ('UVFDAT: flagged',F9.3,' percent valid samples',
     *   ' which were pure 0')
 1302 FORMAT ('UVFDAT: found',I11,' spectra samples already flagged')
 1303 FORMAT ('UVFDAT: found',F11.3,' percent samples already flagged')
 1310 FORMAT ('UVFDAT: omitted',I11,' totally flagged vis records')
 1970 FORMAT ('UVFDAT: ERROR READING TAPE. IERR=', I6)
 1980 FORMAT ('UVFDAT: ERROR WRITTING TO DISK. IERR=', I6)
      END
      SUBROUTINE UVFHIS (ISLOT, IHLUN, IERR)
C-----------------------------------------------------------------------
C   UVFHIS reads the tape (which must be open and positioned at begin.
C   of file) and builds a history file from the FITS history and other
C   keywords in the FITS header.  The Antenna file is also created at
C   this time and the antenna information found in the history is put
C   in this file.  This program leaves the tape positioned at the
C   start of the binary data.  It should work on any random-parameter
C   FITS tape, but is intended primarily for UV data.
C   INPUT:  ISLOT  I        catalog slot of UV data file.
C           IHLUN  I        history file logical unit number.
C   Output: IERR   I        =0 => ok
C                           other => quit
C-----------------------------------------------------------------------
      CHARACTER KL*80, SYMBOL*8, LNAM*12, LCLAS*6, CARD*80, HILINE*72,
     *   CHTMP*8, FITBLK*2880
      DOUBLE PRECISION    DABLK(128)
      REAL      ABLK(256)
      INTEGER   IHBLK(256), IABLK(512), IERR, ISLOT, ICARD, IREC,
     *   ITABNO, IALUN, INC, IST, I, NOANT, NPNT, NN, NNSTR, IHLUN,
     *   IHERR, JERR, I4
      LOGICAL   UPDATE, END, ISHIST, EQUAL
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DABLK, ABLK, IABLK)
      INCLUDE 'INCS:VFUV.INC'
      DATA UPDATE /.TRUE./
      DATA IALUN /30/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Init number of ant records.
      NOANT = 0
      NN = NCT + NKT
      NNSTR = NCT + 1
C                                       Create HI file
      IF (CONCAT.EQ.1) MSGSUP = 32000
      CALL HICREA (IHLUN, DISOUT, ISLOT, CATBLK, IHBLK, IHERR)
      MSGSUP = 0
      IF (IHERR.NE.0) GO TO 950
C                                       Header msg in HI
         WRITE (HILINE,1000) ('----', I = 1,17)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
      IF (CONCAT.NE.1) THEN
         WRITE (HILINE,1002)
      ELSE
         WRITE (HILINE,1003)
         END IF
      IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
C                                       Read record 1 from tape.
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 960
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       Skip required cards.
      ICARD = ICEND
C                                       Loop until END card found.
      DO 30 IREC = 1,100000000
         ICARD = ICARD + 1
C                                       Read next non-blank record.
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 960
C                                       Parse card, put unknown cards
C                                       in history file.
         INC = (ICARD-1) * 80 + 1
         CHTMP = FITBLK(INC:INC+7)
         EQUAL = ('HISTORY'.EQ.CHTMP) .OR. ('COMMENT'.EQ.CHTMP) .OR.
     *      (' '.EQ.CHTMP)
         IST = 1
         IF (EQUAL) IST = 9
         IF (CONCAT.EQ.1) THEN
            IF (CHTMP.EQ.'END     ') GO TO 40
            INC = (ICARD - 1) * 80  +  1
            CARD = FITBLK(INC:INC+79)
            IF (CARD(:15).EQ.'HISTORY AIPS   ') THEN
               IF ((CARD(16:21).EQ.'IPIECE') .OR.
     *            (CARD(16:21).EQ.'FIRSTV') .OR.
     *            (CARD(16:21).EQ.'LASTVI')) THEN
                  IF (IHERR.EQ.0) CALL HIAD80 (IHLUN, IST, CARD, IHBLK,
     *               IHERR)
                  END IF
               END IF
         ELSE
            NPNT = 1
            CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL,
     *         SYMBOL, ITABNO, ISHIST, END, JERR)
            IF (END) GO TO 40
            IF ((JERR.NE.0) .OR. (IST.NE.1)) THEN
               INC = (ICARD - 1) * 80  +  1
               CARD = FITBLK(INC:INC+79)
C                                       special header keywords
               IF (JERR.EQ.-1) THEN
                  CALL PUTCRD (CARD, DISOUT, ISLOT, JERR)
                  IF (JERR.GT.1) THEN
                     WRITE (MSGTXT,1010) JERR
                     CALL MSGWRT (7)
                     IERR = JERR
                     GO TO 999
                     END IF
C                                       Antenna record.
               ELSE IF ((ISHIST) .AND. (SYMBOL(:4).EQ.'ANT ')) THEN
                  CALL ANTREC (KL, NPNT, IALUN, ISLOT, NOANT, IABLK,
     *               JERR)
C                                       Add this unknown card to hist.
               ELSE
                  IF (IHERR.EQ.0) CALL HIAD80 (IHLUN, IST, CARD, IHBLK,
     *               IHERR)
                  END IF
               END IF
            END IF
 30      CONTINUE
C                                       Read more cards than expected.
      GO TO 970
C                                       End card found.
C                                       Clean up antenna file stuff.
 40   IF (NOANT.EQ.0) GO TO 70
C                                       Close ant file, save last recrd
      CALL TABIO ('CLOS', 1, I4, BUFF1, IABLK, JERR)
C                                       Add history info
 70   IF (CONCAT.NE.1) THEN
         WRITE (HILINE,1070)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
         WRITE (HILINE,1072) RLSNAM, ('----', I = 1,11)
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IHERR)
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAM)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLAS)
         IF (IHERR.EQ.0) CALL HENCOO (TSKNAM, LNAM, LCLAS,
     *      CATBLK(KIIMS), FVOL(1), IHLUN, IHBLK, IHERR)
         END IF
C                                       If DISK file give name.
      IF (INFILE.NE.' ') THEN
         WRITE (MSGTXT,1074) TSKNAM, INFILE
         IF (IHERR.EQ.0) CALL HIADD (IHLUN, HILINE, IHBLK, IERR)
         END IF
      CALL HICLOS (IHLUN, UPDATE, IHBLK, I)
      IF (IHERR.EQ.0) GO TO 999
C                                       Error handling.
C                                       History file error.
 950  WRITE (MSGTXT,1950)
      GO TO 990
C                                       Tape read problem.
 960  WRITE (MSGTXT,1960)
      GO TO 990
C                                       Read more cards than we
C                                       expected.
 970  WRITE (MSGTXT,1970)
C
 990  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('/',17A4)
 1002 FORMAT ('/Begin "HISTORY" information found in FITS tape ',
     *   'header by UVLOD')
 1003 FORMAT ('/Begin concatanated "HISTORY" information from FITS ',
     *   'header by UVLOD')
 1010 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1070 FORMAT ('/End FITS tape header "HISTORY" information')
 1072 FORMAT ('UVLOD RELEASE= ''',A7,' '' /',11A4)
 1074 FORMAT (A6,' INFILE=''',A,'''')
 1950 FORMAT ('HISTORY FILE PROBLEM.')
 1960 FORMAT ('TAPE PROBLEM DURING HISTORY FILE READ.')
 1970 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT AN END CARD.')
      END
      SUBROUTINE ANTREC (KL, NPNT, IALUN, ISLOT, NOANT, IABLK, IERR)
C-----------------------------------------------------------------------
C   ANTREC will parse the part of a antenna record history card
C   after 'HISTORY AIPS_or_CVAX ANT' and put the values it finds in an
C   antenna record and write the antenna record to the antenna
C   extension file.  If no antenna file exists (NOANT equals zero)
C   then one will be created.
C   Inputs:
C      KL     C*80     history card.
C      NPNT   I        index into KL after HISTORY AIPS ANT has been
C                      parsed.
C      IALUN  I        logical unit number for antenna file.
C      ISLOT  I        Catalog slot number for UV catalog file.
C   In/out:
C      NOANT  I        Current number of antenna records.
C      IABLK  I(512)   Antenna file I/O buffer.
C   Output:
C      IERR   I        error code. 0=ok. A message will be printed if
C                      an error occurs.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, ANTTAB(5)*8, IDUMMY*2, KL*80, SYMBOL*8,
     *   STR*80
      DOUBLE PRECISION X, C, JD, GASTM
      INTEGER   IABLK(512), NPNT, IERR, NOANT, INTDUM, IALUN,
     *   ITABNO, NCHAR, I, ISLOT, VER, JLOCF, KERR
      LOGICAL   ISHIST, END
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHFREQ /'FREQ    '/
      DATA C /.2997924562D0/
      DATA ANTTAB /'N       ','X       ','Y       ',
     *             'Z       ','ST      '/
C-----------------------------------------------------------------------
C                                       Create antenna file.
      IF (NOANT.GT.0) GO TO 100
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
         ANFQID = -1
         ANTNIF = 2
C                                       Position of the earth's pole
         POLRXY(1) = 0.0
         POLRXY(2) = 0.0
         UT1UTC = 0.0
         DATUTC = 0.0
         TIMSYS = 'IAT'
C                                       Array name
         CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
         ARRAYC(1) = 0.0D0
         ARRAYC(2) = 0.0D0
         ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
         CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
         CALL JULDAY (RDATE, JD)
         CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
         CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), JLOCF, KERR)
         SAFREQ = CATD(KDCRV+JLOCF)
         VER = 1
         XYZHAN = ' '
         TFRAME = ' '
C                                       Create/init file
         CALL ANTINI ('WRIT', IABLK, DISOUT, ISLOT, VER, CATBLK, IALUN,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       init basic AN record
         ANNAME = '        '
         STAXOF = 0.0
         STAXYZ(1) = 0.0D0
         STAXYZ(2) = 0.0D0
         STAXYZ(3) = 0.0D0
         ORBPRM(1) = 0.0D0
         NOSTA = 0
         MNTSTA = 0
         POLAA = 0.0
         POLAB = 0.0
         CALL RFILL (3, 0.0, POLCA)
         CALL RFILL (3, 0.0, POLCB)
         POLTYA = 'R'
         POLTYB = 'L'
         DIAMAN = 25.
         FWHMAN(1) = 0.0
         FWHMAN(2) = 0.0
         DIAMAN = 0.0
         CALL RFILL (MAXIF, 0.0, FWHMAN)
C                                       Write NULL antenna record
         DO 10 I = 1,30
            IANRNO = NOSTA
            CALL TABAN ('WRIT', IABLK, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
            IF (IERR.NE.0) GO TO 999
 10         CONTINUE
C                                       Parse rest of antenna card.
 100  DO 200 I = 1,5
         CALL GETCRD (INTDUM, 5, 1, ANTTAB, IDUMMY, NPNT, KL, SYMBOL,
     *      ITABNO, ISHIST, END, IERR)
         IF (IERR.EQ.1) GO TO 200
         IF (IERR.NE.0) GO TO 980
         IF (ITABNO.NE.5) THEN
            CALL GETNUM (KL, 80, NPNT, X)
            IF (X.EQ.DBLANK) GO TO 975
            END IF
         GO TO (110, 120, 130, 140, 150), ITABNO
C                                       Station number.
 110        CONTINUE
               NOSTA = X + .5
               GO TO 200
C                                       X
 120        CONTINUE
               STAXYZ(1) = X * C
               GO TO 200
C                                       Y
 130        CONTINUE
               STAXYZ(2) = X * C
               GO TO 200
C                                       Z
 140        CONTINUE
               STAXYZ(3) = X * C
               GO TO 200
C                                       Station name.
 150           CALL GETSTR (KL, 80, 4, NPNT, STR, NCHAR)
               NCHAR = MIN (4, NCHAR)
               ANNAME(1:NCHAR) = STR(1:NCHAR)
 200     CONTINUE
C                                       Write antenna record.
         IANRNO = NOSTA
         CALL TABAN ('WRIT', IABLK, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Increment no. of ant records.
      NOANT = NOANT + 1
      GO TO 999
C                                       Incorrect ant rec format.
 975  MSGTXT = 'ANTREC: INVALID NUMBER VALUE'
      IERR = 1
      GO TO 990
 980  MSGTXT = 'ANTREC: INVALID ANTENNA RECORD CARD FORMAT'
 990  CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE UVFEXT (CNO, HLUN, IRET)
C-----------------------------------------------------------------------
C   UVFEXT processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.  This is for the old tables of AIPS style.
C   Inputs:
C      CNO    I         Catalog slot number of UV file.
C      HLUN   I         LUN of open history file
C   Output:
C      IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, ISTR*80, SYM*8, CARD*80, CTYPES(1)*4,
     *   SYMS(15)*8, TABNAM*8, TTYPE(10)*8, FITBLK*2880
      DOUBLE PRECISION C, X, DABLK(1), JD, DEG2RD, DELDAT, GASTM
      REAL      ABLK(1)
      INTEGER   HLUN, HBUFF(256), IRET, IOFF, IERR, TERR, TABVER, J,
     *   TABCNT, TABWID, TABCRD, NC, INC, NPNT, ITYP, NSYMS, NCHAR, NIF,
     *   ITABL, NTYPES, IT, LUN, BUFFER(512), IP, I, CNO, IREC
      LOGICAL   HISERR, NODATA
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (BUFFER, ABLK, DABLK, BUFF2)
      DATA NSYMS, NTYPES, LUN /15, 1, 28/
      DATA CHFREQ /'FREQ    '/
      DATA CTYPES /'  AN'/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA C /.2997924562D0/
C-----------------------------------------------------------------------
      IERR = 0
      TERR = 0
      IRET = 0
      DELDAT = 0.1D0
      DEG2RD = 3.141592653589793D0 / 180.0D0
      IF (TABLES.LE.0) GO TO 910
      CALL HIOPEN (HLUN, DISOUT, CNO, HBUFF, IERR)
      HISERR = IERR.NE.0
      IRET = 8
C                                       Loop over tables
      DO 300 ITABL = 1,TABLES
         WRITE (MSGTXT,1000) ITABL
         IF (.NOT.HISERR) CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
         IF (IERR.NE.0) HISERR = .TRUE.
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = ' '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
C                                       1st Block read in EXTREQ.
            IF (IREC.EQ.1) GO TO 50
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
 30            NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
C                                       card loop
 50         NC = NC + 1
            IF (NC.GT.36) GO TO 90
C                                       card to history
            INC = (NC-1) * 80 + 1
            CARD = FITBLK(INC:INC+79)
            IF (HISERR) GO TO 55
               IF (CARD(1:4).NE.'END ') CALL HIAD80 (HLUN, 1, CARD,
     *            HBUFF, IERR)
               HISERR = IERR.NE.0
C                                       Parse
 55         NPNT = 1
            CALL GETSYM (CARD, NPNT, SYM, ITYP)
            IF (SYM.EQ.'END ') GO TO 100
C                                       only keyword = value accepted
            IF (ITYP.NE.0) GO TO 50
            DO 60 I = 1,NSYMS
               IF (SYM.EQ.SYMS(I)) GO TO 70
 60            CONTINUE
            GO TO 50
C                                       Numeric keywords
 70         IF (I.LE.11) GO TO 80
               CALL GETNUM (CARD, 80, NPNT, X)
               IF (X.EQ.DBLANK) GO TO 875
               IF (I.EQ.12) TABVER = X + 0.01
               IF (I.EQ.13) TABCNT = X + 0.01
               IF (I.EQ.14) TABWID = X + 0.01
               IF (I.EQ.15) TABCRD = X + 0.01
               GO TO 50
C                                       Got a string variable
 80         CONTINUE
               CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
               NCHAR = MIN (NCHAR, 8)
               IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
               IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
               GO TO 50
 90         CONTINUE
         WRITE (MSGTXT,1090) ITABL
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.GT.0) .AND. (TABWID.GT.0)) GO TO 105
            WRITE (MSGTXT,1100) ITABL
            CALL MSGWRT (6)
            GO TO 270
C                                       illegal format
 105     IF ((TABCRD.GT.0) .AND. (TABCRD.LE.40)) GO TO 110
            WRITE (MSGTXT,1105) TABCRD, ITABL
            GO TO 890
C                                       A recognized type?
 110     NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - AN files only
 120     IF (IT.NE.1) GO TO 125
C                                       Setup for AN table initization
            NUMORB = 0
            NOPCAL = 2
            ANFQID = -1
C                                       Position of the earth's pole
            POLRXY(1) = 0.0
            POLRXY(2) = 0.0
            UT1UTC = 0.0
            DATUTC = 0.0
            TIMSYS = 'IAT'
C                                       Array name
            CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
            ARRAYC(1) = 0.0D0
            ARRAYC(2) = 0.0D0
            ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
            CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
            CALL JULDAY (RDATE, JD)
            CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
C                                       Get frequency
            IOFF = 0
            CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), IOFF, IERR)
            SAFREQ = CATD(KDCRV+IOFF)
C                                       Number of values in each column
C                                       in the AN table.
            MSGSUP = 32000
            CALL UVPGET (IERR)
            MSGSUP = 0
            NIF = 1
            IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
            ANTNIF = NIF
C                                       init basic AN record
            ANNAME = '        '
            STAXOF = 0.0
            STAXYZ(1) = 0.0D0
            STAXYZ(2) = 0.0D0
            STAXYZ(3) = 0.0D0
            ORBPRM(1) = 0.0D0
            NOSTA = 0
            MNTSTA = 0
            POLAA = 0.0
            POLAB = 0.0
            CALL RFILL (3, 0.0, POLCA)
            CALL RFILL (3, 0.0, POLCB)
            POLTYA = 'R'
            POLTYB = 'L'
            XYZHAN = ' '
            TFRAME = ' '
            DIAMAN = 0.0
            CALL RFILL (MAXIF, 0.0, FWHMAN)
C                                       Create/init file
            CALL ANTINI ('WRIT', BUFFER, DISOUT, CNO, TABVER, CATBLK,
     *         LUN, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY,
     *         SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME,
     *         XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IERR)
            IF (IERR.EQ.0) GO TO 125
               WRITE (MSGTXT,1120) IERR
               CALL MSGWRT (7)
C
 125     IP = TABCRD
         NODATA = .FALSE.
         DO 260 I = 1,TABCNT
            DO 250 J = 1,TABWID
               IP = IP + 1
               IF (IP.LE.TABCRD) GO TO 140
                  NC = NC + 1
C                                       read a record
                  IF (NC.LE.36) GO TO 130
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), FITBLK)
                     IF (TERR.EQ.0) GO TO 130
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
C                                       spread new card
 130              IP = 1
                  IF (NODATA) GO TO 140
                     INC = (NC-1) * 80 + 1
                     CARD = FITBLK(INC:INC+79)
                     NPNT = 1
 140              IF (NODATA) GO TO 250
C                                       Set proper Ant rec variable.
                     IF (J.NE.2) THEN
                        CALL GETNUM (CARD, 80, NPNT, X)
                        IF (X.EQ.DBLANK) GO TO 875
                        END IF
                     GO TO (150, 160, 170, 180, 190), J
C                                       Station number.
 150                    NOSTA = X + .5
                        GO TO 250
C                                       Station name.
 160                    CALL GETSTR (CARD, 80, 8, NPNT, ISTR, NCHAR)
                        ANNAME = ISTR(1:NCHAR)
C                                       STATION X
 170                    STAXYZ(1) = C * X
                        GO TO 250
C                                       STATION Y
 180                    STAXYZ(2) = C * X
                        GO TO 250
C                                       STATION Z
 190                    STAXYZ(3) = C * X
 250              CONTINUE
               IF (NODATA) GO TO 260
                  IANRNO = NOSTA
                  CALL TABAN ('WRIT', BUFFER, IANRNO, ANKOLS, ANNUMV,
     *               ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *               DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB,
     *               POLAB, POLCB, IERR)
                  IF (IERR.NE.0) GO TO 900
 260           CONTINUE
C                                       Close ant file.
               CALL TABIO ('CLOS', 1, IANRNO, BUFFER, BUFFER, IERR)
               WRITE (MSGTXT,1267) 'AN', TABVER
               CALL MSGWRT (2)
               GO TO 300
C                                       Data ignored
 270        CONTINUE
               WRITE (MSGTXT,1270) ITABL
               CALL MSGWRT (2)
               IF (HISERR) GO TO 300
                  CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
                  HISERR = IERR.NE.0
 300     CONTINUE
      IRET = 0
      GO TO 900
C
 875  MSGTXT = 'FITEXT: NUMBER ERROR ON ' // SYM
      IRET = 1
C
 890  CALL MSGWRT (8)
C                                       OK if EOF found
      IF (TERR.EQ.4) IRET = 0
C                                       Read rest of tape
 900  CALL HICLOS (HLUN, .TRUE., HBUFF, IERR)
 910  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVLOD  / Header for table',I7)
 1020 FORMAT ('UVFEXT: FITS IO ERROR',I7)
 1090 FORMAT ('UVFEXT: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('UVFEXT: TABCARDS=',I7,' ILLEGAL')
 1120 FORMAT ('UVFEXT: UNABLE TO CREATE EXTENSION FILE',I7)
 1267 FORMAT ('Extension file type ',A2,' version',I4,' written')
 1270 FORMAT ('UVLOD / Table',I7,' skipped')
      END
      SUBROUTINE FITRXU (CNO, HLUN, LAST, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with UV
C  data and process the ones it recognizes (XTENSION = 'TABLE'
C  and 'BINTABLE' (or temporary names 'A3DTABLE' & '3D TABLE').
C  Inputs:
C     CNO     I    Catalog number of the UV file.
C     HLUN    I    History file LUN. Already open.
C     LAST    L    Last piece?
C  OUTPUTS:
C     EOF     L    An end of file was read during processing.
C     IERR    I    Error code. 0=ok.
C-----------------------------------------------------------------------
      INTEGER   CNO, HLUN, IERR
      LOGICAL   LAST, EOF
C                                       MXTBKW=max. no. table keywords
      INTEGER   MXTBKW
      PARAMETER (MXTBKW = 200)
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TAB3D(3)*8,
     *   FITBLK*2880, HILINE*72
      HOLLERITH KEYH(2)
      DOUBLE PRECISION  NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR
      INTEGER   I, II, ICARD, ANLUN, INBLK, IKEY, HBUFF(256), IVER,
     *   TABLUN, SRTORD, DATP(128,2), BUFFER(512), KEYI, NUMKEY,
     *   KEYTYP(MXTBKW), KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5),
     *   JERR, JTRIM, JT
      LOGICAL   EXTEN, KEYL, DOHDR, WASSU
      INCLUDE 'UVLOD.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TAB3D /'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA ANLUN, TABLUN /28, 29/
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
      CALL HIOPEN (HLUN, DISOUT, CNO, HBUFF, IERR)
      WASSU = .FALSE.
C                                       Loop for all FITS extensions.
      DO 200 I = 1,32000
         NUMKEY = 0
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, FITBLK, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF (.NOT.EXTEN) GO TO 900
         IF (IERR.NE.0) GO TO 910
C                                       Calculate no. of data blocks.
         AXCNT = 1.0D0
         DO 20 II = 1,NAXIS
            AXCNT = AXCNT * NAXISI(II)
 20         CONTINUE
         AXCNT = AXCNT + PCOUNT
         NBITS = ABS (BITPIX) * GCOUNT * AXCNT
         INBLK = INT ((NBITS + 23039.0D0) / 23040.0D0)
         DOHDR = .FALSE.
C                                       See if we have an ASCII table.
         IF (EXTTYP.NE.'TABLE') GO TO 40
            IF (NAXISI(1).LE.2880) GO TO 25
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (8)
               GO TO 100
C                                       initialize default values.
 25         CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 0, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 910
C                                       Process AN table and write
C                                       AN file records.
            IF (ITYPE.NE.'AN') GO TO 30
               CALL ANTAB (FDVEC, TBIND, DISOUT, CNO, ANLUN, NAXISI,
     *            TAPBUF, IERR)
C                                       Add AIPS AN history
               WRITE (MSGTXT,1025)
               CALL EXTHIS (HLUN, HBUFF, IERR)
               GO TO 200
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
 30            SRTORD = 0
               IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD,
     *            KEYVAL, KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
               IVER = 0
               CALL MAKTAB (SRTORD, DISOUT, CNO, IVER, CATBLK, TABLUN,
     *            DATP, BUFFER, IERR)
               IF (IERR.GT.0) GO TO 110
C                                       Prepare keywords
               IKEY = 1
               DO 35 II = 1,NUMKEY
                  KEYLOC(II) = IKEY
                  IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
                  IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
                  IF (KEYTYP(II).EQ.3) THEN
                     JT = JTRIM (KEYCHR(II))
                     CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                     END IF
                  IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
                  IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
                  CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
                  IKEY = IKEY + LENKEY(KEYTYP(II))
 35               CONTINUE
C                                       Write keywords
               CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *            KEYV, KEYTYP, IERR)
               IF (IERR.NE.0) THEN
                  CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                  GO TO 110
                  END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
               CALL RWTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *            IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               GO TO 200
C                                       See if we have a 3-D table.
 40      IF ((EXTTYP.NE.TAB3D(1)) .AND. (EXTTYP.NE.TAB3D(2)) .AND.
     *      (EXTTYP.NE.TAB3D(3))) GO TO 100
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 1, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, FITBLK, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 910
            IF (ITYPE.NE.'UV') THEN
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
               SRTORD = 0
               IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *            KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
               IVER = 0
               IF (UVTABL) IVER = EXTVER
               CALL MAKTAB (SRTORD, DISOUT, CNO, IVER, CATBLK, TABLUN,
     *            DATP, BUFFER, IERR)
               IF (IERR.GT.0) GO TO 110
C                                       file already exists
               IF ((UVTABL) .AND. (IERR.EQ.0)) THEN
                  IF ((ITYPE.NE.'CL') .AND. (ITYPE.NE.'SN') .AND.
     *               (ITYPE.NE.'TY') .AND. (ITYPE.NE.'MC') .AND.
     *               (ITYPE.NE.'IM') .AND. (ITYPE.NE.'BP')) THEN
                     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                     GO TO 110
                     END IF
                  END IF
C                                       Prepare keywords
               IF ((IERR.EQ.-1) .OR. (.NOT.UVTABL)) THEN
                  IERR = 0
                  IKEY = 1
                  DO 80 II = 1,NUMKEY
                     KEYLOC(II) = IKEY
                     IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
                     IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
                     IF (KEYTYP(II).EQ.3) THEN
                        JT = JTRIM (KEYCHR(II))
                        CALL CHR2H (8, KEYCHR(II), 1, KEYH)
                        END IF
                     IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
                     IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
                     CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
                     IKEY = IKEY + LENKEY(KEYTYP(II))
 80                  CONTINUE
C                                       Write keywords
                  CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *               KEYV, KEYTYP, IERR)
                  IF (IERR.NE.0) THEN
                     CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, IERR)
                     GO TO 110
                     END IF
                  END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
               CALL R3DTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *            IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               IF (ITYPE.EQ.'SU') WASSU = .TRUE.
C                                       UV data table
            ELSE
               CALL RUVTAB (NPV, FDVEC, TBIND, NAXISI, DISOUT, CNO,
     *            LAST, NSKIP, DOKEEP, BUFF1, BUFF2, TAPBUF, IERR)
               EOF = EOF .OR. (IERR.EQ.4)
               IF (IERR.NE.0) GO TO 910
               END IF
            GO TO 200
C                                       Skip unknown extension file.
 100     CONTINUE
C                                       read rest header code
            DOHDR = .TRUE.
C                                       else header already read
 110        CALL SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *         TAPBUF, FITBLK, IERR)
            IF (IERR.NE.0) GO TO 910
 200     CONTINUE
C                                       Shouldn't get here.
      MSGTXT = 'MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED'
      CALL MSGWRT (8)
C                                       zero records flagged
 900  IF (NZERO(1,1).GT.0) THEN
         WRITE (HILINE,1900) TSKNAM, NZERO(1,1), 'cross'
         CALL HIADD (HLUN, HILINE, HBUFF, JERR)
         END IF
      IF (NZERO(1,2).GT.0) THEN
         WRITE (HILINE,1900) TSKNAM, NZERO(1,2), 'auto'
         CALL HIADD (HLUN, HILINE, HBUFF, JERR)
         END IF
 910  CALL HICLOS (HLUN, .TRUE., HBUFF, JERR)
C                                       source table apparent positions
      IF (WASSU) CALL SUPREC (DISOUT, CNO, CATBLK, JERR)
      CALL CATIO ('UPDT', DISOUT, CNO, CATBLK, 'CLWR', SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1910) JERR, 'CATBLK UPDATE'
         CALL MSGWRT (8)
         END IF
      NCFILE = 0
      IERR = MAX (0, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITRXU: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1025 FORMAT ('UVLOD / AIPS antennas table')
 1900 FORMAT (A,'NZERO=',I10,' / ',A,'-corr spectra flagged as pure 0')
 1910 FORMAT ('ERROR',I5,' DOING ',A)
      END
      SUBROUTINE ANTAB (FDVEC, TBIND, IVOL, ICNO, ANLUN, NAXIS, TAPBUF,
     *   IERR)
C-----------------------------------------------------------------------
C  This routine will read the data section of a FITS extension file
C  of type TABLE and with the EXTNAM of AIPS AN (antenna), decode this
C  information using data obtained from the header section of the
C  extension file, and write the AIPS version of the ANtenna file.
C  Inputs:
C     FDVEC    I(50)     File descriptor vector for TAPIO input
C     IVOL     I         Disk volume number of map.
C     ICNO     I         Catalog number of map.
C     ANLUN    I         AIPS LUN to use for AN file
C     NAXIS    I(2)      Length of card, number of cards in table.
C     IHDR     I(256)    Catalog header of map.
C  Input/output:
C     TAPBUF   I(*)      TAPIO buffer
C     TBIND    I         Buffer pointer in TAPBUF
C  Outputs:
C     IERR     I        Error code. 0=ok.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER  CHFREQ*8, LINE*1024, TLINE*1024, ANTNO*8, STATON*8,
     *   LX*8, LY*8, LZ*8
      INTEGER   NAXIS(2), IVOL, ICNO, ANLUN, FDVEC(40), TBIND, IERR,
     *   TAPBUF(*), JT, JTRIM, NIF
      DOUBLE PRECISION X, DABLK(128), JD, DEG2RD, DELDAT, GASTM
      REAL      ANBUF(1)
      INTEGER   NCTR, EXTVER, INDEXX, I, NAXIS1, NAXIS2, LCTR, NPNTR,
     *   IWIDTH, IFRAC, IANBUF(512), TORDER(5), IOFF
      LOGICAL   EQUAL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (DABLK, ANBUF, IANBUF)
      DATA CHFREQ /'FREQ'/
      DATA ANTNO, STATON /'ANT NO. ','STATION '/
      DATA LX, LY, LZ /'LX      ','LY      ','LZ      '/
C-----------------------------------------------------------------------
      NAXIS1 = NAXIS(1)
      NAXIS2 = NAXIS(2)
C
      DEG2RD = 3.141592653589793D0 / 180.0D0
      DELDAT = 0.1D0
C                                       Setup for AN table initization
      NUMORB = 0
      NOPCAL = 2
C                                       Position of the earth's pole
      POLRXY(1) = 0.0
      POLRXY(2) = 0.0
      UT1UTC = 0.0
      DATUTC = 0.0
      TIMSYS = 'IAT'
C                                       Array name
      CALL H2CHR (8, 1, CATH(KHTEL), ANAME)
C                                       Array center (rel to center of
C                                       earth)
      ARRAYC(1) = 0.0D0
      ARRAYC(2) = 0.0D0
      ARRAYC(3) = 0.0D0
C                                       Get GST0 and Earth rotation rate
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD)
      CALL GSTROT (JD, GSTIA0, GASTM, DEGPDY)
C                                       Get frequency
      IOFF = 0
      CALL AXEFND (4, CHFREQ, KICTPN, CATH(KHCTP), IOFF, IERR)
      SAFREQ = CATD(KDCRV+IOFF)
C                                       init basic AN record
      ANNAME = '        '
      STAXOF = 0.0
      STAXYZ(1) = 0.0D0
      STAXYZ(2) = 0.0D0
      STAXYZ(3) = 0.0D0
      ORBPRM(1) = 0.0D0
      NOSTA = 0
      MNTSTA = 0
      POLAA = 0.0
      POLAB = 0.0
      CALL RFILL (3, 0.0, POLCA)
      CALL RFILL (3, 0.0, POLCB)
      POLTYA = 'R'
      POLTYB = 'L'
      ANFQID = -1
      MSGSUP = 32000
      CALL UVPGET (IERR)
      MSGSUP = 0
      NIF = 1
      IF (JLOCIF.GE.0) NIF = MAX (1, CATBLK(KINAX+JLOCIF))
      ANTNIF = NIF
      XYZHAN = ' '
      TFRAME = ' '
C                                       Create/init file
      EXTVER = 0
      CALL ANTINI ('WRIT', ANBUF, IVOL, ICNO, EXTVER, CATBLK, ANLUN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 999
      NCTR = 2881
C                                       Determine order of standard
C                                       values on card.
      DO 100 I = 1,ITNCOL
         IF (TTYPE(I).EQ.ANTNO) TORDER(1) = I
         IF (TTYPE(I).EQ.STATON) TORDER(2) = I
         IF (TTYPE(I).EQ.LX) TORDER(3) = I
         IF (TTYPE(I).EQ.LY) TORDER(4) = I
         IF (TTYPE(I).EQ.LZ) TORDER(5) = I
 100     CONTINUE
      DIAMAN = 0.0
      CALL RFILL (MAXIF, 0.0, FWHMAN)
C                                       Loop for all lines in table.
      DO 800 LCTR = 1, NAXIS2
C                                       Read a FITS table data line.
         CALL TABLIN (NAXIS1, FDVEC, TBIND, NCTR, TAPBUF, TLINE, IERR)
         LINE(1:NAXIS1) = TLINE(1:NAXIS1)
         IF (IERR.NE.0) GO TO 999
         DO 200 I = 1,5
C                                       Decode the 5 data fields.
            INDEXX = TORDER(I)
            NPNTR = TBCOL(INDEXX)
            IWIDTH = TWIDTH(INDEXX)
            GO TO (150, 120, 150, 150, 150) , I
 120        CONTINUE
C                                       Station name.
               ANNAME = TLINE(NPNTR:NPNTR+IWIDTH-1)
               JT = JTRIM (ANNAME)
               GO TO 200
C                                       Station No. or Position.
 150        CONTINUE
               IFRAC = TFRAC(INDEXX)
               X = 0.0D0
               EQUAL = TLINE(NPNTR:NPNTR+NAXIS1-1) .EQ.
     *            TNULL(INDEXX)(1:NAXIS1)
               IF (.NOT.EQUAL)
     *            CALL DCODEF (NPNTR, IWIDTH, IFRAC, LINE, X, IERR)
               IF (I.EQ.1) NOSTA = X + .01
               IF (I.EQ.3) STAXYZ(1) =  X
               IF (I.EQ.4) STAXYZ(2) =  X
               IF (I.EQ.5) STAXYZ(3) =  X
 200        CONTINUE
C                                       Write the AIPS AN record.
         IANRNO = NOSTA
         CALL TABAN ('WRIT', ANBUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
C
 800     CONTINUE
      CALL TABIO ('CLOS', 1, IANRNO, ANBUF, ANBUF, IERR)
C
 999  RETURN
      END
      SUBROUTINE DELRPM (IOFF)
C-----------------------------------------------------------------------
C   Delete the random parameter at offset IOFF from the random parameter
C   list (0 <= IOFF < CATBLK(KIPCN)). If the indicated parameter is at
C   the end of the random parameter list then the number of random
C   parameters is decreased by one, otherwise the name of the random
C   parameter is set to 'REMOVED '.
C
C   Inputs:
C     IOFF       I       Offset of random parameter to delete
C
C   Input/Output in common:
C     CATBLK     I(*)    UV file header
C     CATH       H(*)    UV file header
C     KIPCN      I       CATBLK(KIPCN) is the number of random
C                        parameters
C     KHPTP      I       The list of random parameter names starts at
C                        CATH(KHPTP)
C-----------------------------------------------------------------------
      INTEGER   IOFF

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (IOFF .EQ. CATBLK(KIPCN)-1) THEN
         CATBLK(KIPCN) = CATBLK(KIPCN) - 1
      ELSE
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+2*IOFF))
         END IF
C
 999  RETURN
      END
      SUBROUTINE APPRPM (NAME)
C-----------------------------------------------------------------------
C   Append a random parameter with name NAME to the end of the random
C   parameter list.
C
C   Input:
C     NAME       C*8       Name of new random parameter
C
C   Input/output in common:
C     CATBLK     I*(*)     UV data header
C     CATH       H*(*)     UV data header
C     KIPCN      I         CATBLK(KIPCN) is the number of random
C                          parameters
C     KHPTP      I         The list of random parameter names starts at
C                          CATH(KHPTP)
C-----------------------------------------------------------------------
      CHARACTER NAME*8

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL CHR2H (8, NAME, 1, CATH(KHPTP + 2 * CATBLK(KIPCN)))
      CATBLK(KIPCN) = CATBLK(KIPCN) + 1
C
 999  RETURN
      END
      SUBROUTINE SUPREC (DISK, CNO, CATBLK, IERR)
C-----------------------------------------------------------------------
C   SUPREC re-writes the SU table, precessing the coordinates of epoch
C   to approximate apparent coordinates when J2000 is being used.
C   Input:
C      DISK     I      Disk number
C      CNO      I      Catalog number
C      CATBLK   I(*)   Catalog header
C   Output:
C      IERR     I       Error code
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO, CATBLK(256), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), LUN, NUMIF, FREQID, ISURNO, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), IDSOU, QUAL, VER, NREC, IREC
      REAL      FLUX(4,MAXIF), POLAR(2)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EQUINX, JD0,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   OBSPOS(3), RAOBS, DECOBS
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, RDATE*8
      LOGICAL   ISOPEN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA OBSPOS, POLAR /3*0.0D0, 2*0.0/
C-----------------------------------------------------------------------
      IERR = 0
      CALL FNDEXT ('SU', CATBLK, VER)
      IF (VER.LE.0) GO TO 999
      LUN = 57
      ISOPEN = .FALSE.
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         GO TO 990
         END IF
      ISOPEN = .TRUE.
      NREC = BUFFER(5)
      CALL H2CHR (8, 1, CATBLK(KHDOB), RDATE)
      CALL JULDAY (RDATE, JD0)
      DO 100 IREC = 1,NREC
         ISURNO = IREC
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
C                                       compute apparents pos
         CALL JPRECS (JD0, EQUINX, 1.0D-6, 1, .TRUE., OBSPOS,
     *      POLAR, DG2RAD * RAEPO, DG2RAD * DECEPO, RAAPP, DECAPP)
         RAAPP = RAD2DG * RAAPP
         DECAPP = RAD2DG * DECAPP
         IF (RAAPP.LT.0.0D0) RAAPP = RAAPP + 360.0D0
C                                       put back in file
         ISURNO = IREC
         CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRIT'
            GO TO 990
            END IF
 100     CONTINUE
      GO TO 995
C
 990  CALL MSGWRT (7)
 995  IF (ISOPEN) CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *   IDSOU,SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *   DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *   PMRA, PMDEC, IREC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUPREC ERROR',I4,' ON ',A,' FIXING APPARENT COORDINATES')
      END
      SUBROUTINE SUCHCK (DISK, CNO)
C-----------------------------------------------------------------------
C   SUCHCK re-writes the SU table changing duplicate source names
C   Input:
C      DISK   I   Disk number
C      CNO    I   Catalog number
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO
C
      INTEGER   MAXSOU
      PARAMETER (MAXSOU = 1000)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   BUFFER(512), LUN, NUMIF, FREQID, ISURNO, SUKOLS(MAXSUC),
     *   SUNUMV(MAXSUC), IDSOU, QUAL, VER, NREC, IREC, IERR, JTRIM,
     *   SNUM(MAXSOU), LUNTMP, JREC, J, CHANGE, SQUAL(MAXSOU)
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION FREQO(MAXIF), BANDW, RAEPO, DECEPO, EQUINX,
     *   RAAPP, DECAPP, LSRVEL(MAXIF), LRESTF(MAXIF), PMRA, PMDEC,
     *   RAOBS, DECOBS
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4,
     *   SNAM(MAXSOU)*16, SCALC(MAXSOU)*4
      LOGICAL   ISOPEN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      IERR = 0
      CALL FNDEXT ('SU', CATBLK, VER)
      IF (VER.LE.0) GO TO 999
      LUN = LUNTMP (1)
      ISOPEN = .FALSE.
      CALL SOUINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN, NUMIF,
     *   VELTYP, VELDEF, FREQID, ISURNO, SUKOLS, SUNUMV, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT'
         GO TO 990
         END IF
      ISOPEN = .TRUE.
      NREC = BUFFER(5)
C                                       read in names
      DO 20 IREC = 1,NREC
         ISURNO = IREC
         CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
         J = JTRIM (SOUNAM)
         SNAM(IREC) = SOUNAM
         SNUM(IREC) = IDSOU
         SQUAL(IREC) = QUAL
         SCALC(IREC) = CALCOD
 20      CONTINUE
C                                       chck for duplicates
      CHANGE = 0
 30   DO 50 IREC = 1,NREC-1
         DO 40 JREC = IREC+1,NREC
            IF ((SNAM(IREC).EQ.SNAM(JREC)) .AND.
     *         (SNUM(IREC).NE.SNUM(JREC)) .AND.
     *         (SCALC(IREC).EQ.SCALC(JREC)) .AND.
     *         (SQUAL(IREC).EQ.SQUAL(JREC))) THEN
               ISURNO = JREC
               CALL TABSOU ('READ', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'RE-READ'
                  GO TO 990
                  END IF
               QUAL = QUAL + 1
               SQUAL(JREC) = QUAL
               ISURNO = JREC
               CALL TABSOU ('WRIT', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *            IDSOU, SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, LRESTF, PMRA, PMDEC, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'RE-READ'
                  GO TO 990
                  END IF
               CHANGE = CHANGE + 1
               WRITE (MSGTXT,1030) IDSOU, SOUNAM, QUAL
               CALL MSGWRT (3)
               GO TO 30
               END IF
 40         CONTINUE
 50      CONTINUE
      IF (CHANGE.GT.0) THEN
         WRITE (MSGTXT,1050) CHANGE
         CALL MSGWRT (2)
         END IF
      GO TO 995
C
 990  CALL MSGWRT (7)
 995  IF (ISOPEN) CALL TABSOU ('CLOS', BUFFER, ISURNO, SUKOLS, SUNUMV,
     *   IDSOU,SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO,
     *   DECEPO, EQUINX, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, LRESTF,
     *   PMRA, PMDEC, IREC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SUCHCK ERROR',I4,' ON ',A,' CHECKING SOURCE NAMES')
 1030 FORMAT ('SUCHCK: source number',I5,' named ',A,' new qual',I5)
 1050 FORMAT ('SUCHCK: changed',I4,' total duplicate source names')
      END
      SUBROUTINE UVZERO (CATBLK, VIS, NZERO, GOOD)
C-----------------------------------------------------------------------
C   Checks for all freq channels pure zero in real and imag and flags
C   them
C   Inputs:
C      CATBLK   I(*)     Header
C   In/out
C      VIS      R(3,*)   data
C      NZERO    I(3)     Number spectra all zero counter, (3) total,
C                        (2) previously flagged
C      GOOD     L        Some good data?
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), NZERO(3)
      REAL      VIS(3,*)
      LOGICAL   GOOD
C
      INTEGER   NS, NIF, NF, NC, JNCIF, JNCF, JNCS, I, J, JIF, JS, JF
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       pointers to traverse the data
      NS = 1
      NIF = 1
      NF = 1
      NC = CATBLK(KINAX)
      JNCIF = INCIF / NC
      JNCF  = INCF / NC
      JNCS  = INCS / NC
      IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
      DO 40 JIF = 1,NIF
         DO 30 JS = 1,NS
            J = (JIF-1) * JNCIF + (JS-1) * JNCS + 1
            I = J - JNCF
            NZERO(3) = NZERO(3) + 1
C                                       all already flagged?
            DO 10 JF = 1,NF
               I = I + JNCF
               IF (VIS(3,I).GT.0.0) GO TO 15
 10            CONTINUE
            NZERO(2) = NZERO(2) + 1
            GO TO 30
C                                      all valid are zero?
 15         I = J - JNCF
            DO 20 JF = 1,NF
               I = I + JNCF
               IF ((VIS(3,I).GT.0.0) .AND. ((VIS(1,I).NE.0.0) .OR.
     *            (VIS(2,I).NE.0.0))) GO TO 30
 20            CONTINUE
C                                       all zero: flag
            NZERO(1) = NZERO(1) + 1
            I = J - JNCF
            DO 25 JF = 1,NF
               I = I + JNCF
               VIS(3,I) = MIN (0.0, -ABS(VIS(3,I)))
 25            CONTINUE
 30         CONTINUE
 40      CONTINUE
C                                       look again, anything good
      GOOD = .TRUE.
      DO 70 JIF = 1,NIF
         DO 60 JS = 1,NS
            J = (JIF-1) * JNCIF + (JS-1) * JNCS + 1
            I = J - JNCF
C                                       all already flagged?
            DO 50 JF = 1,NF
               I = I + JNCF
               IF (VIS(3,I).GT.0.0) GO TO 999
 50            CONTINUE
 60         CONTINUE
 70      CONTINUE
      GOOD = .FALSE.
C
 999  RETURN
      END

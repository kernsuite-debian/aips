LOCAL INCLUDE 'MATCH.INC'
C                                       Local include for MATCH
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXSOU
      PARAMETER (MAXSOU=512)
C
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAME2(3), XCLAS2(2), XNAMOU(3),
     *   XCLAOU(2)
      REAL      XSIN, XDISIN, XS2, XDISK2, XSOUT, XDISO, BUFF1(UVBFSS),
     *   BUFF2(UVBFSS), XFLAGS
      INTEGER   SEQ1, SEQ2, SEQO, DISK1, DISK2, DISKO, JBUFSZ, ILOCWT,
     *   CAT1(256), CAT2(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, CNO1, CNO2, CNOO,
     *   ANTS(MAXANT,100), SOURCS(MAXSOU), FQIDS(MAXFQ), WASA, WASS,
     *   WASF, IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP, DOFR, DOAN, DOSO
      CHARACTER NAMEIN*12, CLAIN*6, NAME2*12, CLAS2*6, NAMOUT*12,
     *   CLAOUT*6, ANAMES(MAXANT,100)*8
      DOUBLE PRECISION FRQ1, FRQ2
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAME2, XCLAS2,
     *   XS2, XDISK2, XNAMOU, XCLAOU, XSOUT, XDISO, XFLAGS
      COMMON /MATCHC/ CAT1, CAT2, FRQ1, FRQ2, SEQ1, SEQ2, SEQO, DISK1,
     *   DISK2, DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, JBUFSZ, CNO1,
     *   CNO2, CNOO, ANTS, SOURCS, FQIDS, WASA, WASS, WASF, DOFR, DOAN,
     *   DOSO
      COMMON /CHARPM/ NAMEIN, CLAIN, NAME2, CLAS2, NAMOUT, CLAOUT,
     *   ANAMES
      COMMON /BUFRS/ BUFF1, BUFF2
      INCLUDE 'INCS:DCAT.INC'
C                                       End local include for MATCH
LOCAL END
      PROGRAM MATCH
C-----------------------------------------------------------------------
C! Converts antenna, source, FQ numbers of one data set to match 2nd
C# UV VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2000-2001, 2007-2009, 2012, 2015
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
C   MATCH renumbers the antennas, sources, and FQs of one data set to
C   match those of another.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQ1          Seq. of input UV data.
C      INDISK         DISK1         Disk number of input VU data.
C      IN2NAME        NAME2         Name of Master file.
C      IN2CLASS       CLAS2         Class of Master file.
C      IN2SEQ         SEQ2          Seq. no. of Master file.
C      IN2DISK        DISK2         Vol. no. of Master file.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQO          Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'MATCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'MATCH '/
C-----------------------------------------------------------------------
C                                       Get inputs create output file
      CALL MATCIN (PRGM, IRET)
C                                       Get table translations
      IF (IRET.EQ.0) CALL MATCTA (IRET)
C                                       Translate data to output file
      IF (IRET.EQ.0) CALL MATCUV (IRET)
C                                       Write HIstory info
      IF (IRET.EQ.0) CALL MATCHI
C                                       Close down files, etc.
      CALL DIE (IRET, IBUFF1)
C
 999  STOP
      END
      SUBROUTINE MATCIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   MATCIN gets input parameters for MATCH and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
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
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, J
      LOGICAL   T
      INCLUDE 'MATCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /' '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      J = 100 * MAXANT
C                                       zero translation array
      CALL FILL (J, 0, ANTS)
      CALL FILL (MAXSOU, 0, SOURCS)
      CALL FILL (MAXFQ, 0, FQIDS)
      WASA = 0
      WASS = 0
      WASF = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 22
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, IBUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQ1 = IROUND (XSIN)
      SEQO = IROUND (XSOUT)
      SEQ2 = IROUND (XS2)
      DISK1 = IROUND (XDISIN)
      DISK2 = IROUND (XDISK2)
      DISKO = IROUND (XDISO)
C                                       flags
      J = IROUND (XFLAGS)
      IF (J.LE.0) J = 7
      DOAN = MOD(J,2).GT.0
      J = J / 2
      DOSO = MOD(J,2).GT.0
      J = J / 2
      DOFR = MOD(J,2).GT.0
C                                       Get CATBLK from Master file
      CNO2 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, CNO2, NAME2, CLAS2, SEQ2, PTYPE,
     *   NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAME2, CLAS2, SEQ2, DISK2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, CNO2, CATBLK, 'REST', IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save master CATBLK
      CALL COPY (256, CATBLK, CAT2)
      CALL UVPGET (IERR)
      FRQ2 = FREQ
C                                       Get CATBLK from old file.
      CNO1 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK1, CNO1, NAMEIN, CLAIN, SEQ1,
     *   PTYPE, NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQ1, DISK1,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK1, CNO1, CATBLK, 'REST', IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CAT1)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
C                                       Find weight and scale.
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      FRQ1 = FREQ
      IF (DOFR) CATD(KDCRV+JLOCF) = FRQ2
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQ1, BLANK, NAMOUT, CLAOUT, SEQO)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO
C                                       Create output file.
      CNOO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CNOO, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CNOO.NE.CNO1) .OR. (DISKO.NE.DISK1)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CNOO, CATBLK, 'WRIT', IBUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       copy keywords
      CALL KEYCOP (DISK1, CNO1, DISKO, CNOO, IERR)
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISK1, CNO1, NAMEIN, CLAIN, SEQ1,
     *   PTYPE, NLUSER, 'READ', IBUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
      JERR = 0
      SEQO = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MATCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('MATCIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE MATCTA (IRET)
C-----------------------------------------------------------------------
C   MATCTA determines the translation of antenna, source, FQ numbers
C   from input file 1 to match file 2
C   Outputs:
C      IRET     I      Error code: > 0 quit
C   Common output:
C      ANTS     I(*,*)   Antenna number out (ant # in, subarray)
C      SOURCS   I(*)     Source number out (source number in)
C      FQIDS    I(*)     FQ ID # out (FQ IF # in)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'MATCH.INC'
      INTEGER   IV, NV, NV1, NV2, LUNI, LUNO, NREC, IREC, JREC, MAXA, I,
     *   J, IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQ), FQID, IFSIDE(MAXIF),
     *   FFSIDE(MAXIF,MAXFQ), NUMIF, ISURNO, SUKOLS(MAXSUC), LUN3,
     *   SUNUMV(MAXSUC), IBUFF3(512)
      CHARACTER EXTYPE*2, STNAME(MAXANT)*8, SSNAME(MAXSOU)*16,
     *   BNDCOD(MAXIF)*8
      DOUBLE PRECISION SXYZ(3,MAXANT), AEPS, FEPS, IFFREQ(MAXIF),
     *   FFREQ(MAXIF,MAXFQ), SRA(MAXSOU), SDEC(MAXSOU), PEPS,
     *   ORA(MAXSOU), ODEC(MAXSOU)
      REAL      IFCHW(MAXIF), IFTBW(MAXIF), FFCHW(MAXIF,MAXFQ), FEPS2,
     *   BUFF3(512)
      LOGICAL   OPEN3
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (IBUFF3, BUFF3)
      DATA LUNI, LUNO, LUN3 /21,22,23/
      DATA AEPS, FEPS, FEPS2, PEPS /1.0D0, 1.0D2, 1.0E0, 2.778D-7/
C-----------------------------------------------------------------------
C                                       Antenna matching
      EXTYPE = 'AN'
      CALL FNDEXT (EXTYPE, CAT1, NV1)
      CALL FNDEXT (EXTYPE, CAT2, NV2)
      IF (DOAN) THEN
         IF (NV1.NE.NV2) THEN
            MSGTXT = 'WARNING: NUMBER OF SUBARRAYS DOES NOT MATCH'
            CALL MSGWRT (7)
            END IF
      ELSE
         MSGTXT = 'WARNING: ANTENNAS NOT MATCHED'
         CALL MSGWRT (7)
         NV2 = 0
         END IF
      NV = MAX (NV1, NV2)
C                                       Loop over all antenna files
      DO 90 IV = 1,NV
C                                       Open file to copy
         IF (IV.LE.NV2) THEN
            CALL ANTINI ('READ', IBUFF1, DISK2, CNO2, IV, CAT2, LUNI,
     *         IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *         RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *         TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
            IF (IRET.GT.0) GO TO 999
            CALL ANTINI ('READ', IBUFF3, DISK1, CNO1, IV, CAT1, LUN3,
     *         IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *         RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *         TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
            OPEN3 = .TRUE.
         ELSE
            CALL ANTINI ('READ', IBUFF1, DISK1, CNO1, IV, CAT1, LUNI,
     *         IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *         RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *         TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
            OPEN3 = .FALSE.
            END IF
         IF (IRET.GT.0) GO TO 999
         NREC = IBUFF1(5)
         CALL ANTINI ('WRIT', IBUFF2, DISKO, CNOO, IV, CATBLK, LUNO,
     *      IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ,
     *      RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *      TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Copy file to output, common
         DO 10 NOSTA = 1,MAXANT
            STNAME(NOSTA) = 'EMPTYANT'
            ANTS(NOSTA,IV) = 0
            ANAMES(NOSTA,IV) = '????????'
 10         CONTINUE
         MAXA = 0
         DO 20 IREC = 1,NREC
            IANRNO = IREC
            CALL TABAN ('READ', IBUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) GO TO 999
            IANRNO = IREC
            CALL TABAN ('WRIT', IBUFF2, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
            IF (IRET.NE.0) GO TO 999
            MAXA = MAX (MAXA, NOSTA)
            STNAME(NOSTA) = ANNAME
            ANAMES(NOSTA,IV) = ANNAME
            SXYZ(1,NOSTA) = STAXYZ(1)
            SXYZ(2,NOSTA) = STAXYZ(2)
            SXYZ(3,NOSTA) = STAXYZ(3)
 20         CONTINUE
C                                       close input
         CALL TABIO ('CLOS', 1, IANRNO, IBUFF1, BUFF1, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       non-matching: 1st has more
         IF (IV.GT.NV2) THEN
            DO 30 NOSTA = 1,MAXANT
               IF (STNAME(NOSTA).NE.'EMPTYANT') ANTS(NOSTA,IV) = NOSTA
 30            CONTINUE
C                                       read 1st and match
         ELSE
            JREC = IBUFF3(5)
            IF (JREC.NE.NREC) THEN
               WRITE (MSGTXT,1030) IV, JREC, NREC
               CALL MSGWRT (6)
               END IF
            DO 50 IREC = 1,JREC
               IANRNO = IREC
               CALL TABAN ('READ', IBUFF3, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF,
     *            DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB,
     *            POLCB, IRET)
               IF (IRET.NE.0) GO TO 999
               DO 40 I = 1,MAXA
                  IF ((ANNAME.EQ.STNAME(I)) .AND.
     *               (ABS(SXYZ(1,I)-STAXYZ(1)).LT.AEPS) .AND.
     *               (ABS(SXYZ(2,I)-STAXYZ(2)).LT.AEPS) .AND.
     *               (ABS(SXYZ(3,I)-STAXYZ(3)).LT.AEPS)) THEN
                     ANTS(NOSTA,IV) = I
                     GO TO 50
                     END IF
 40               CONTINUE
C                                       append an extra antenna!
               WRITE (MSGTXT,1040) IREC, IV
               CALL MSGWRT (6)
               NREC = NREC + 1
               MAXA = MAXA + 1
               ANTS(NOSTA,IV) = MAXA
               ANAMES(MAXA,IV) = ANNAME
               IANRNO = NREC
               NOSTA = MAXA
               CALL TABAN ('WRIT', IBUFF2, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               STNAME(NOSTA) = ANNAME
               SXYZ(1,NOSTA) = STAXYZ(1)
               SXYZ(2,NOSTA) = STAXYZ(2)
               SXYZ(3,NOSTA) = STAXYZ(3)
 50            CONTINUE
            END IF
C                                       close input
         IF (OPEN3) THEN
            CALL TABIO ('CLOS', 1, IANRNO, IBUFF3, BUFF3, IRET)
            IF (IRET.GT.0) GO TO 999
            END IF
C                                       close output
         CALL TABIO ('CLOS', 1, IANRNO, IBUFF2, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 90      CONTINUE
C                                       FQ tables
      EXTYPE = 'FQ'
      CALL FNDEXT (EXTYPE, CAT1, NV1)
      CALL FNDEXT (EXTYPE, CAT2, NV2)
      IF (DOFR) THEN
         IF (NV1.NE.NV2) THEN
            MSGTXT = 'WARNING: NUMBER OF FQ TABLES DOES NOT MATCH'
            CALL MSGWRT (7)
            END IF
      ELSE
         MSGTXT = 'WARNING: FREQUENCIES NOT MATCHED'
         CALL MSGWRT (7)
         NV2 = 0
         END IF
      NV = MIN (1, MAX (NV1, NV2))
C                                       Do 1 or 0 FQ tables
      DO 190 IV = 1,NV
C                                       Open file to copy
         IF (IV.LE.NV2) THEN
            CALL FQINI ('READ', IBUFF1, DISK2, CNO2, IV, CAT2, LUNI,
     *         IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
            IF (IRET.GT.0) GO TO 999
            CALL FQINI ('READ', IBUFF3, DISK1, CNO1, IV, CAT1, LUN3,
     *         IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
            OPEN3 = .TRUE.
         ELSE
            CALL FQINI ('READ', IBUFF1, DISK1, CNO1, IV, CAT1, LUNI,
     *         IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
            OPEN3 = .FALSE.
            END IF
         IF (IRET.GT.0) GO TO 999
         NREC = IBUFF1(5)
         CALL FQINI ('WRIT', IBUFF2, DISKO, CNOO, IV, CATBLK, LUNO,
     *      IFQRNO, FQKOLS, FQNUMV, NUMIF, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Copy file to output, common
         DO 110 I = 1,MAXFQ
            FFREQ(1,I) = -1.0D0
            FQIDS(I) = 0
 110        CONTINUE
         MAXA = 0
         DO 120 IREC = 1,NREC
            IFQRNO = IREC
            CALL TABFQ ('READ', IBUFF1, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            IFQRNO = IREC
            CALL TABFQ ('WRIT', IBUFF2, IFQRNO, FQKOLS, FQNUMV, NUMIF,
     *         FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD, IRET)
            IF (IRET.NE.0) GO TO 999
            MAXA = MAX (MAXA, FQID)
            CALL COPY (NUMIF, IFSIDE, FFSIDE(1,FQID))
            CALL DPCOPY (NUMIF, IFFREQ, FFREQ(1,FQID))
            CALL RCOPY (NUMIF, IFCHW, FFCHW(1,FQID))
 120        CONTINUE
C                                       close input
         CALL TABIO ('CLOS', 1, IFQRNO, IBUFF1, BUFF1, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       non-matching: 1st has more
         IF (IV.GT.NV2) THEN
            DO 130 I = 1,MAXFQ
               IF (FFREQ(1,I).GT.0.0D0) FQIDS(I) = I
 130           CONTINUE
C                                       read 1st and match
         ELSE
            JREC = IBUFF3(5)
            IF (JREC.NE.NREC) THEN
               WRITE (MSGTXT,1130) JREC, NREC
               CALL MSGWRT (6)
               END IF
            DO 150 IREC = 1,JREC
               IFQRNO = IREC
               CALL TABFQ ('READ', IBUFF3, IFQRNO, FQKOLS, FQNUMV,
     *            NUMIF, FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               DO 133 J = 1,NUMIF
                  IFFREQ(J) = IFFREQ(J) + FRQ1 - FRQ2
 133              CONTINUE
               DO 140 I = 1,MAXA
                  DO 135 J = 1,NUMIF
                     IF (FFSIDE(J,I).NE.IFSIDE(J)) GO TO 140
                     IF (ABS(FFREQ(J,I)-IFFREQ(J)).GT.FEPS) GO TO 140
                     IF (ABS(FFCHW(J,I)-IFCHW(J)).GT.FEPS2) GO TO 140
 135                 CONTINUE
                  FQIDS(FQID) = I
                  GO TO 150
 140              CONTINUE
C                                       append an extra FQID!
               WRITE (MSGTXT,1140) IREC
               CALL MSGWRT (6)
               NREC = NREC + 1
               MAXA = MAXA + 1
               FQIDS(FQID) = MAXA
               IFQRNO = NREC
               FQID = MAXA
               CALL TABFQ ('WRIT', IBUFF2, IFQRNO, FQKOLS, FQNUMV,
     *            NUMIF, FQID, IFFREQ, IFCHW, IFTBW, IFSIDE, BNDCOD,
     *            IRET)
               IF (IRET.NE.0) GO TO 999
               CALL COPY (NUMIF, IFSIDE, FFSIDE(1,FQID))
               CALL DPCOPY (NUMIF, IFFREQ, FFREQ(1,FQID))
               CALL RCOPY (NUMIF, IFCHW, FFCHW(1,FQID))
 150           CONTINUE
            END IF
C                                       close input
         IF (OPEN3) THEN
            CALL TABIO ('CLOS', 1, IFQRNO, IBUFF3, BUFF3, IRET)
            IF (IRET.GT.0) GO TO 999
            END IF
C                                       close output
         CALL TABIO ('CLOS', 1, IFQRNO, IBUFF2, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 190     CONTINUE
C                                       SU tables
      EXTYPE = 'SU'
      CALL FNDEXT (EXTYPE, CAT1, NV1)
      CALL FNDEXT (EXTYPE, CAT2, NV2)
      IF (DOSO) THEN
         IF (NV1.NE.NV2) THEN
            MSGTXT = 'WARNING: NUMBER OF SU TABLES DOES NOT MATCH'
            CALL MSGWRT (7)
            END IF
      ELSE
         MSGTXT = 'WARNING: SOURCES NOT MATCHED'
         CALL MSGWRT (7)
         NV2 = 0
         END IF
      NV = MIN (1, MAX (NV1, NV2))
C                                       Do 1 or 0 FQ tables
      DO 290 IV = 1,NV
C                                       Open file to copy
         IF (IV.LE.NV2) THEN
            CALL SOUINI ('READ', IBUFF1, DISK2, CNO2, IV, CAT2, LUNI,
     *         NUMIF, VELTYP, VELDEF, FQID, ISURNO, SUKOLS, SUNUMV,
     *         IRET)
            IF (IRET.GT.0) GO TO 999
            CALL SOUINI ('READ', IBUFF3, DISK1, CNO1, IV, CAT1, LUN3,
     *         NUMIF, VELTYP, VELDEF, FQID, ISURNO, SUKOLS, SUNUMV,
     *         IRET)
            OPEN3 = .TRUE.
         ELSE
            CALL SOUINI ('READ', IBUFF1, DISK1, CNO1, IV, CAT1, LUNI,
     *         NUMIF, VELTYP, VELDEF, FQID, ISURNO, SUKOLS, SUNUMV,
     *         IRET)
            OPEN3 = .FALSE.
            END IF
         IF (IRET.GT.0) GO TO 999
         NREC = IBUFF1(5)
         CALL SOUINI ('WRIT', IBUFF2, DISKO, CNOO, IV, CATBLK, LUNO,
     *      NUMIF, VELTYP, VELDEF, FQID, ISURNO, SUKOLS, SUNUMV, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Copy file to output, common
         DO 210 I = 1,MAXSOU
            SSNAME(I) = 'NOT A SOURCE'
            SOURCS(I) = 0
 210        CONTINUE
         MAXA = 0
         DO 220 IREC = 1,NREC
            ISURNO = IREC
            CALL TABSOU ('READ', IBUFF1, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            ISURNO = IREC
            CALL TABSOU ('WRIT', IBUFF2, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *         SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IRET)
            IF (IRET.NE.0) GO TO 999
            MAXA = MAX (MAXA, IDSOUR)
            SSNAME(IDSOUR) = SNAME
            SRA(IDSOUR) = RAEPO
            SDEC(IDSOUR) = DECEPO
            ORA(IDSOUR) = RAOBS
            ODEC(IDSOUR) = DECOBS
 220        CONTINUE
C                                       close input
         CALL TABIO ('CLOS', 1, ISURNO, IBUFF1, BUFF1, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       non-matching: 1st has more
         IF (IV.GT.NV2) THEN
            DO 230 I = 1,MAXSOU
               IF (SSNAME(I).NE.'NOT A SOURCE') SOURCS(I) = I
 230           CONTINUE
C                                       read 1st and match
         ELSE
            JREC = IBUFF3(5)
            IF (JREC.NE.NREC) THEN
               WRITE (MSGTXT,1230) JREC, NREC
               CALL MSGWRT (6)
               END IF
            DO 250 IREC = 1,JREC
               ISURNO = IREC
               CALL TABSOU ('READ', IBUFF3, ISURNO, SUKOLS, SUNUMV,
     *            IDSOUR, SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) GO TO 999
               DO 240 I = 1,MAXA
                  IF ((SSNAME(I).EQ.SNAME) .AND.
     *               (ABS(SRA(I)-RAEPO).LT.PEPS) .AND.
     *               (ABS(SDEC(I)-DECEPO).LT.PEPS) .AND.
     *               (ABS(ORA(I)-RAOBS).LT.PEPS) .AND.
     *               (ABS(ODEC(I)-DECOBS).LT.PEPS)) THEN
                     SOURCS(IDSOUR) = I
                     GO TO 250
                     END IF
 240              CONTINUE
C                                       append an extra Source ID!
               WRITE (MSGTXT,1240) IREC
               CALL MSGWRT (6)
               NREC = NREC + 1
               MAXA = MAXA + 1
               SOURCS(IDSOUR) = MAXA
               ISURNO = NREC
               IDSOUR = MAXA
               CALL TABSOU ('WRIT', IBUFF2, ISURNO, SUKOLS, SUNUMV,
     *            IDSOUR, SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW,
     *            RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, RAOBS, DECOBS,
     *            LSRVEL, RESTFQ, PMRA, PMDEC, IRET)
               IF (IRET.NE.0) GO TO 999
               SSNAME(IDSOUR) = SNAME
               SRA(IDSOUR) = RAEPO
               SDEC(IDSOUR) = DECEPO
 250           CONTINUE
            END IF
C                                       close input
         IF (OPEN3) THEN
            CALL TABIO ('CLOS', 1, ISURNO, IBUFF3, BUFF3, IRET)
            IF (IRET.GT.0) GO TO 999
            END IF
C                                       close output
         CALL TABIO ('CLOS', 1, ISURNO, IBUFF2, BUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 290     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('Subarray',I3,' no ants in file 1',I3,' file 2',I3,
     *   ' not same')
 1040 FORMAT ('ANT',I3,' SUBARR',I3,' HAS NO MATCH - APPEND IT')
 1130 FORMAT ('Warning: number FQIDs file 1',I4,' file 2',I4,
     *   ' not same')
 1140 FORMAT ('FQID',I4,' HAS NO MATCH - APPEND IT')
 1230 FORMAT ('Warning: number sources file 1',I4,' file 2',I4,
     *   ' not same')
 1240 FORMAT ('SUID',I4,' HAS NO MATCH - APPEND IT')
      END
      SUBROUTINE MATCUV (IRET)
C-----------------------------------------------------------------------
C   MATCUV sends uv data one point at a time to the user supplied
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
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY, ISUB
      LOGICAL   T, F
      INCLUDE 'MATCH.INC'
      REAL      BASEN, CBUFF(UVBFSS), RESULT(UVBFSS), UVMULT
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
      UVMULT = 1.0
      IF (DOFR) UVMULT = FRQ2 / FRQ1
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CAT1(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISK1, CNO1, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISK1, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, OFILE, IRET)
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
               ISUB = 100.0 * (BASEN - IA1*256 - IA2) + 1.5
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               ISUB = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                       Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL DIDDLE (NUMVIS, IA1, IA2, ISUB, CBUFF, BUFF1(IPTRI),
     *            INCX, RESULT, IRET)
C                                       Un compressed data
            ELSE
               CALL DIDDLE (NUMVIS, IA1, IA2, ISUB, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI), INCX, RESULT, IRET)
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
               BUFF2(IPTRO+ILOCU) = BUFF2(IPTRO+ILOCU) * UVMULT
               BUFF2(IPTRO+ILOCV) = BUFF2(IPTRO+ILOCV) * UVMULT
               BUFF2(IPTRO+ILOCW) = BUFF2(IPTRO+ILOCW) * UVMULT
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       OK: w or w/o output
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
C                                       Final call to DIDDLE.
 200     NUMVIS = -1
         CALL DIDDLE (NUMVIS, IA1, IA2, ISUB, BUFF1, BUFF1, INCX,
     *      RESULT, IRET)
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
      CALL UCMPRS (NVIS, DISKO, CNOO, LUNO, CATBLK, IRET)
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
 1000 FORMAT ('MATCUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('MATCUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('MATCUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('MATCUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('MATCUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('MATCUV: DIDDLE ERROR',I3)
 1150 FORMAT ('MATCUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE MATCHI
C-----------------------------------------------------------------------
C   MATCHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(3)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, J, NONOT
      LOGICAL   T
      INCLUDE 'MATCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /3, 'AN', 'FQ', 'SU'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISK1, DISKO, CNO1, CNOO, CATBLK, IBUFF1,
     *   IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQ1, DISK1, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQO, DISKO, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       antenna translations
      DO 20 J = 1,100
         DO 10 I = 1,MAXANT
            IF ((ANTS(I,J).GT.0) .AND. (ANTS(I,J).NE.I)) THEN
               WRITE (HILINE,1010) TSKNAM, I, J, ANTS(I,J)
               CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
               IF (IERR.NE.0) GO TO 200
               END IF
 10         CONTINUE
 20      CONTINUE
C                                       source translations
      DO 30 I = 1,MAXSOU
         IF ((SOURCS(I).GT.0) .AND. (SOURCS(I).NE.I)) THEN
            WRITE (HILINE,1020) TSKNAM, I, SOURCS(I)
            CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
 30      CONTINUE
C                                       FQ translations
      DO 40 I = 1,MAXFQ
         IF ((FQIDS(I).GT.0) .AND. (FQIDS(I).NE.I)) THEN
            WRITE (HILINE,1030) TSKNAM, I, FQIDS(I)
            CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
 40      CONTINUE
C                                       Frequency
      IF ((ABS(FRQ1-FRQ2).GT.50.D0) .AND. (DOFR)) THEN
         WRITE (HILINE,1040) TSKNAM, FRQ1, FRQ2
         CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, IBUFF2, IERR)
C                                       Warnings
      IF (WASA.GT.0) THEN
         WRITE (MSGTXT,1200) WASA, 'ANTENNA'
         CALL MSGWRT (6)
         END IF
      IF (WASS.GT.0) THEN
         WRITE (MSGTXT,1200) WASS, 'SOURCE'
         CALL MSGWRT (6)
         END IF
      IF (WASF.GT.0) THEN
         WRITE (MSGTXT,1200) WASF, 'FQ ID'
         CALL MSGWRT (6)
         END IF
C                                       Copy other tables
      CALL ADJTAB (NONOT, NOTTYP, LUN1, LUN2, DISK1, DISKO, CNO1, CNOO,
     *   ANTS, ANAMES, SOURCS, FQIDS, CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'MATCHI: ERROR COPYING TABLES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('MATCHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Antenna',I3,' Subarray',I3,' becomes antenna',I3)
 1020 FORMAT (A6,'/ Source',I4,' becomes source number',I4)
 1030 FORMAT (A6,'/ FQ ID number',I4,' becomes FQ ID number',I4)
 1040 FORMAT (A6,'/ Freq',1PD17.9,' becomes freq',1PD17.9)
 1200 FORMAT (I8,' UNDEFINED TRANSLATIONS FOR ',A,'S')
      END
      SUBROUTINE ADJTAB (NONOT, NOTTYP, LUNOLD, LUNNEW, VOLOLD, VOLNEW,
     *   CNOOLD, CNONEW, ANTS, ANAMES, SOURCS, FQIDS, CATNEW, BUFF1,
     *   BUFF2, IRET)
C-----------------------------------------------------------------------
C   ADJTAB copies all Table extension file(s).  The output files must
C   be new - old ones cannot be rewritten.  The output file must be
C   opened WRIT in the catalog and will have its CATBLK updated on disk.
C   Source, FQ, and antenna numbers are renumbered.
C   Inputs:
C      NONOT   I       Number of "Forbidden" types to copy.
C      NOTTYP  C*2(*)  Table types to ignore.
C      LUNOLD  I       LUN for old file
C      LUNNEW  I       LUN for new file
C      VOLOLD  I       Disk number for old file.
C      VOLNEW  I       Disk number for new file.
C      CNOOLD  I       Catalog slot number for old file
C      CNONEW  I       Catalog slot number for new file
C      ANTS    I(*,*)  New antenna numbers
C      ANAMES  C(*,*)  New antenna names
C      SOURCS  I(*)    New source numbers
C      FQIDS   I(*)    New FQ numbers
C   In/out:
C      CATNEW  I(256)  Catalog header for new file.
C   Output:
C      BUFF1   I(1024) Work buffer
C      BUFF2   I(1024) Work buffer
C      IRET    I       Return error code  0 => ok, otherwise TABCOP
C                                             or 10*CATIO error.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER NOTTYP(*)*2, ANAMES(MAXANT,*)*8
      INTEGER   NONOT, LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD, CNONEW,
     *   ANTS(MAXANT,*), SOURCS(*), FQIDS(*), BUFF1(*), BUFF2(*),
     *   CATNEW(256), IRET
C
      INCLUDE 'INCS:PHDR.INC'
      INTEGER   NKOLS
      PARAMETER (NKOLS=9)
      INTEGER   IVERI, IVERO, IER, NONON, IEXT, JEXT, OLDNUM(NIEXTN),
     *   NKEY, NCOL, DATP(128,2), NREC, KOLS(NKOLS), LKEY(NKOLS), RTYPE,
     *   VALUE(MAXIF), ISUB, LRNO, IREC, K, IANT, JTRIM, J, JJ,
     *   ROWB(XBPRSZ)
      LOGICAL   TABLE, EXIST, FITASC, DOPUT, DIDSOM, DIDONE
      CHARACTER OLDTYP(NIEXTN)*2, NONTAB(20)*2, KEYS(NKOLS)*8, ANNAME*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C                                       Non table extension files
      DATA NONON /3/
      DATA NONTAB /'HI','PL','SL',17*'  '/
      DATA KEYS /'SOURCE', 'ANTENNA', 'REFANT 1', 'REFANT 2',
     *   'ANTS', 'ANNAME', 'FRQSEL','FREQID','FREQ ID'/
      DATA LKEY /6, 7, 8, 8, 8, 8, 8, 8, 8/
C-----------------------------------------------------------------------
C                                       Get old CATBLK in BUFF2.
      CALL CATIO ('READ', VOLOLD, CNOOLD, BUFF2, 'REST', BUFF1, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (6)
         IRET = 10 * IRET
         GO TO 999
         END IF
C                                       header ext format
      CALL FXHDEX (BUFF2)
C                                       Get extension file info
      DO 20 IEXT = 1,KIEXTN
         OLDTYP(IEXT) = ' '
         CALL H2CHR (2, 1, BUFF2(KHEXT+IEXT-1), OLDTYP(IEXT))
         OLDNUM(IEXT) = BUFF2(KIVER+IEXT-1)
C                                       Check if on non table list
         DO 10 JEXT = 1,NONON
            IF (OLDTYP(IEXT).EQ.NONTAB(JEXT)) OLDNUM(IEXT) = 0
 10         CONTINUE
C                                       Check "forbidden" types
         IF (NONOT.GT.0) THEN
            DO 15 JEXT = 1,NONOT
               IF (OLDTYP(IEXT).EQ.NOTTYP(JEXT)) OLDNUM(IEXT) = 0
 15            CONTINUE
            END IF
 20      CONTINUE
C                                       Loop, copying tables.
      DO 100 IEXT = 1,KIEXTN
         IF (OLDNUM(IEXT).LE.0) GO TO 100
C                                       Copy each table independently
         DO 90 JEXT = 1,OLDNUM(IEXT)
C                                       See if files exist and are
C                                       wanted.
            CALL ISTAB (OLDTYP(IEXT), VOLOLD, CNOOLD, JEXT, LUNOLD,
     *         BUFF1, TABLE, EXIST, FITASC, IER)
            IF (TABLE .AND. EXIST .AND. (IER.EQ.0)) THEN
               IVERI = JEXT
               IVERO = JEXT
C                                       Copy
               CALL TABCOP (OLDTYP(IEXT), IVERI, IVERO, LUNOLD, LUNNEW,
     *            VOLOLD, VOLNEW, CNOOLD, CNONEW, CATNEW, BUFF1, BUFF2,
     *            IER)
               IRET = MAX (IER, IRET)
               IF (IER.NE.0) GO TO 90
C                                       Re-open output to update #s
               DIDSOM = .FALSE.
               NKEY = 0
               NCOL = 0
               CALL TABINI ('WRIT', OLDTYP(IEXT), VOLNEW, CNONEW, IVERO,
     *            CATNEW, LUNNEW, NKEY, NREC, NCOL, DATP, BUFF2, IER)
               IRET = MAX (IER, IRET)
               IF (IER.NE.0) GO TO 90
               NREC = BUFF2(5)
               CALL FILL (NKOLS, 0, KOLS)
               K = 0
               CALL FNDCOL (1, KEYS(1), LKEY(1), .TRUE., BUFF2, KOLS(1),
     *            IER)
               IF ((IER.GT.0) .AND. (IER.LE.10)) GO TO 85
               IF (IER.GT.10) K = K + IER - 10
               CALL FNDCOL (1, KEYS(2), LKEY(2), .TRUE., BUFF2, KOLS(2),
     *            IER)
               IF ((IER.GT.0) .AND. (IER.LE.10)) GO TO 85
               IF (IER.GT.10) K = K + IER - 10
               CALL FNDCOL (NKOLS-2, KEYS(3), LKEY(3), .TRUE., BUFF2,
     *            KOLS(3), IER)
               IF ((IER.GT.0) .AND. (IER.LE.10)) GO TO 85
               IER = IER + K
               IF (IER.GE.10+NKOLS) THEN
                  IER = 0
                  GO TO 85
                  END IF
C                                       Update needed
               IF (KOLS(7).EQ.0) KOLS(7) = KOLS(8)
               IF (KOLS(7).EQ.0) KOLS(7) = KOLS(9)
C                                       subarray too
               KOLS(NKOLS) = 0
               IF ((KOLS(2).GT.0) .OR. (KOLS(3).GT.0) .OR.
     *            (KOLS(4).GT.0) .OR. (KOLS(5).GT.0) .OR.
     *            (KOLS(6).GT.0)) THEN
                  CALL FNDCOL (1, 'SUBARRAY', 7, .TRUE., BUFF2,
     *               KOLS(NKOLS), IER)
                  IF ((IER.GT.0) .AND. (IER.LE.10)) GO TO 85
                  END IF
               DO 80 IREC = 1,NREC
                  LRNO = 0
                  DIDONE = .FALSE.
C                                       source number
                  IF (KOLS(1).GT.0) THEN
                     CALL GETCOL (IREC, KOLS(1), DATP, LRNO, BUFF2,
     *                  RTYPE, VALUE, ROWB, IER)
                     IF (IER.NE.0) GO TO 85
                     IF ((VALUE(1).GT.0) .AND. (SOURCS(VALUE(1)).GT.0)
     *                  .AND. (SOURCS(VALUE(1)).NE.VALUE(1))) THEN
                        VALUE(1) = SOURCS(VALUE(1))
                        CALL PUTCOL (IREC, KOLS(1), DATP, LRNO, BUFF2,
     *                     VALUE, ROWB, IER)
                        IF (IER.NE.0) GO TO 85
                        DIDSOM = .TRUE.
                        DIDONE = .TRUE.
                        END IF
                     END IF
C                                       FQ number
                  IF (KOLS(7).GT.0) THEN
                     CALL GETCOL (IREC, KOLS(7), DATP, LRNO, BUFF2,
     *                  RTYPE, VALUE, ROWB, IER)
                     IF (IER.NE.0) GO TO 85
                     IF ((VALUE(1).GT.0) .AND. (FQIDS(VALUE(1)).GT.0)
     *                  .AND. (FQIDS(VALUE(1)).NE.VALUE(1))) THEN
                        VALUE(1) = FQIDS(VALUE(1))
                        CALL PUTCOL (IREC, KOLS(7), DATP, LRNO, BUFF2,
     *                     VALUE, ROWB, IER)
                        IF (IER.NE.0) GO TO 85
                        DIDSOM = .TRUE.
                        DIDONE = .TRUE.
                        END IF
                     END IF
C                                       subarray number
                  IF (KOLS(NKOLS).GT.0) THEN
                     CALL GETCOL (IREC, KOLS(NKOLS), DATP, LRNO, BUFF2,
     *                  RTYPE, ISUB, ROWB, IER)
                     IF (IER.NE.0) GO TO 85
                     ISUB = MAX (1, ISUB)
                  ELSE
                     ISUB = 1
                     END IF
C                                       antenna number(s)
                  IANT = 0
                  DO 30 K = 2,5
                     IF (KOLS(K).GT.0) THEN
                        CALL GETCOL (IREC, KOLS(K), DATP, LRNO, BUFF2,
     *                     RTYPE, VALUE, ROWB, IER)
                        IF (IER.NE.0) GO TO 85
                        JJ = RTYPE / 10
                        JJ = MAX (1, JJ)
                        DOPUT = .FALSE.
                        DO 25 J = 1,JJ
                           IF ((VALUE(J).GT.0) .AND.
     *                        (ANTS(VALUE(1),ISUB).GT.0) .AND.
     *                        (ANTS(VALUE(J),ISUB).NE.VALUE(J))) THEN
                              VALUE(J) = ANTS(VALUE(J),ISUB)
                              DOPUT = .TRUE.
                              END IF
 25                        CONTINUE
                        IF (DOPUT) THEN
                           CALL PUTCOL (IREC, KOLS(K), DATP, LRNO,
     *                        BUFF2, VALUE, ROWB, IER)
                           IF (IER.NE.0) GO TO 85
                           DIDSOM = .TRUE.
                           DIDONE = .TRUE.
                           END IF
                        IF (K.EQ.2) IANT = VALUE(1)
                        END IF
 30                  CONTINUE
C                                       Station name
                  IF ((KOLS(6).GT.0) .AND. (IANT.GT.0)) THEN
                     CALL GETCOL (IREC, KOLS(6), DATP, LRNO, BUFF2,
     *                  RTYPE, VALUE, ROWB, IER)
                     IF (IER.NE.0) GO TO 85
                     CALL H2CHR (8, 1, VALUE, ANNAME)
                     K = JTRIM (ANNAME)
                     IF (ANNAME.NE.ANAMES(IANT,ISUB)) THEN
                        CALL CHR2H (8, ANAMES(IANT,ISUB), 1, VALUE)
                        CALL PUTCOL (IREC, KOLS(6), DATP, LRNO, BUFF2,
     *                     VALUE, ROWB, IER)
                        IF (IER.NE.0) GO TO 85
                        DIDSOM = .TRUE.
                        DIDONE = .TRUE.
                        END IF
                     END IF
C                                       force write to disk
                  IF (DIDONE) THEN
                     CALL PUTCOL (0, KOLS, DATP, LRNO, BUFF2, VALUE,
     *                  ROWB, IER)
                     IF (IER.NE.0) GO TO 85
                     END IF
 80               CONTINUE
C                                       close table
 85            IRET = MAX (IRET, IER)
               CALL TABIO ('CLOS', 0, NREC, ROWB, BUFF2, IER)
               IRET = MAX (IRET, IER)
               IF (DIDSOM) THEN
                  WRITE (MSGTXT,1085) OLDTYP(IEXT), IVERO
                  CALL MSGWRT (2)
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADJTAB: ERROR',I5,' READING OLD CATBLK')
 1085 FORMAT ('Source, FQ, and/or antenna #s adjusted in ',A,
     *   ' table version',I5)
      END
      SUBROUTINE DIDDLE (NUMVIS, IA1, IA2, ISUB, VIS, RPARM, INCX,
     *   RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      ISUB    I    Subarray number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      NAME2      C*12    Name of the aux. file
C      CLAS2      C*6     Class of the aux. file.
C      SEQ2       I       Sequence number of the aux. file.
C      DISK2      I       Volumn number of the aux. file.
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
C   Output:
C      RPARM      R    Modified random parameter array.
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, ISUB, INCX, IRET
      REAL      VIS(INCX,*), RPARM(*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I, JA1, JA2,
     *   JFQ, JSU, IFQ, ISU, JJS, JJ(4)
      INCLUDE 'MATCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA JJ /1,2,4,3/
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         IF (ILOCSU.GE.0) THEN
            ISU = RPARM(1+ILOCSU) + 0.01
            JSU = SOURCS(ISU)
            IF ((ISU.GT.0) .AND. (JSU.GT.0)) THEN
               RPARM(1+ILOCSU) = JSU
            ELSE
               WASS = WASS + 1
               END IF
            END IF
         IF (ILOCFQ.GE.0) THEN
            IFQ = RPARM(1+ILOCFQ) + 0.01
            JFQ = FQIDS(IFQ)
            IF ((IFQ.GT.0) .AND. (JFQ.GT.0)) THEN
               RPARM(1+ILOCFQ) = JFQ
            ELSE
               WASF = WASF + 1
               END IF
            END IF
         JA1 = ANTS(IA1,ISUB)
         JA2 = ANTS(IA2,ISUB)
         IF (JA1.LE.0) THEN
            JA1 = IA1
            WASA = WASA + 1
            END IF
         IF (JA2.LE.0) THEN
            JA2 = IA2
            WASA = WASA + 1
            END IF
C                                       copy data only
         IF (JA1.LE.JA2) THEN
            IF (ILOCB.GE.0) THEN
               RPARM(1+ILOCB) = 256 * JA1 + JA2 + (ISUB-1)/100.0
            ELSE
               RPARM(1+ILOCA1) = JA1
               RPARM(1+ILOCA2) = JA2
               RPARM(1+ILOCSA) = ISUB
               END IF
            DO 40 JIF = 1,NIF
               DO 30 JF = 1,NF
                  DO 20 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *                  (JS-1) * INCSO + 1
                     DO 10 I = 1,INCX
                        RESULT(I,INDEXO) = VIS(I,INDEXI)
 10                     CONTINUE
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
C                                       reverse things
         ELSE
            RPARM(1+ILOCU) = -RPARM(1+ILOCU)
            RPARM(1+ILOCV) = -RPARM(1+ILOCV)
            RPARM(1+ILOCW) = -RPARM(1+ILOCW)
            IF (ILOCB.GE.0) THEN
               RPARM(1+ILOCB) = 256 * JA2 + JA1 + (ISUB-1)/100.0
            ELSE
               RPARM(1+ILOCA1) = JA1
               RPARM(1+ILOCA2) = JA2
               RPARM(1+ILOCSA) = ISUB
               END IF
            DO 80 JIF = 1,NIF
               DO 70 JF = 1,NF
                  DO 60 JS = 1,NS
                     JJS = JJ(JS)
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *                  (JJS-1) * INCSO + 1
                     RESULT(1,INDEXO) = VIS(1,INDEXI)
                     RESULT(2,INDEXO) = -VIS(2,INDEXI)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 60                  CONTINUE
 70               CONTINUE
 80            CONTINUE
            END IF
C                                       Last call
      ELSE
         END IF
C
 999  RETURN
      END

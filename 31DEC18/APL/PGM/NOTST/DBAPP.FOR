LOCAL INCLUDE 'DBAPP.INC'
C                                       Local include for DBAPP
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MAXSOU, MXANT
C                                       MAXSOU = size of source table
      PARAMETER (MAXSOU=2000)
C                                       MXANT = max no antennas
      PARAMETER (MXANT=MAXANT)
      INTEGER   SEQ1, SEQ2, SEQO, DISKI, DISKO, BUFSZ, CATII(256),
     *   CATO(256), MAXA(2), FIXPOL, POLANT(4), CNOI, CNOO,
     *   I1LOCU, I1LOCV, I1LOCW, I1LOCT, I1LOCB, I1LOCS, I1LOCQ,
     *   I1LOCI, I1LOCC, I1LOCA, I1LOC1, I1LOC2,
     *   J1LOCC, J1LOCS, J1LOCF, J1LOCD, J1LOCR, J1LOCI, INCS1, INCF1,
     *   INCIF1, ICOR01, NPARM1, LREC1, NCOR1, TYUVD1,
     *   I2LOCU, I2LOCV, I2LOCW, I2LOCT, I2LOCB, I2LOCS, I2LOCQ,
     *   I2LOCI, I2LOCC, I2LOCA, I2LOC1, I2LOC2,
     *   J2LOCC, J2LOCS, J2LOCF, J2LOCR, J2LOCD, J2LOCI, INCS2, INCF2,
     *   INCIF2, ICOR02, NPARM2, LREC2, NCOR2, TYUVD2,
     *   IORD(14), JORD(2,MAXCIF), NUMSOU, SOUTRA(MAXSOU),
     *   FQ2TRA(MAXFQ), NEWVIS, TBUFF1(512), TBUFF2(512),
     *   IBUFF1(UVBFSL), IBUFF2(UVBFSL)
      INTEGER   NVIS1, NVIS2, NNIF
      LOGICAL   DOUVMI, ISCMPI, ISCMPO, MULTI, FIXANT
      REAL      XS1, XD1, XS2, XSO, XDO, XFQTOL, DAYOFF,
     *   BUFF1(UVBFSL), BUFF2(UVBFSL), TBUFF(UVBFSS), XBUFF(UVBFSS),
     *   CATRI(256), CATRO(256), DEQUI1, DEQUI2, DOARR, DOPOS
      HOLLERITH XNAME1(3), XCLAS1(2), XNAMEO(3), XCLASO(2),
     *   CATHI(256), CATHO(256)
      CHARACTER NAMEI*12, CLASSI*6, NAMEO*12, CLASSO*6
      DOUBLE PRECISION FREQ1, FREQ2, RA1, RA2, DEC1, DEC2, UVMULT,
     *   CATDI(128), CATDO(128), DELTC, DELTF
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAME1, XCLAS1, XS1, XD1, XS2, XNAMEO, XCLASO,
     *   XSO, XDO, XFQTOL
      COMMON /DBAPPP/ SEQ1, SEQ2, SEQO, DISKI, DISKO, BUFSZ, DAYOFF,
     *   ISCMPI, ISCMPO, MULTI, CNOI, CNOO, NEWVIS, IORD, JORD, MAXA,
     *   FIXPOL, POLANT, NUMSOU, SOUTRA, NNIF, FQ2TRA, DOARR, DOPOS,
     *   FIXANT
      EQUIVALENCE (CATII, CATRI, CATHI, CATDI),
     *   (CATO, CATRO, CATHO, CATDO)
      COMMON /BUFRS/ BUFF1, BUFF2, TBUFF, XBUFF, TBUFF1, TBUFF2
      COMMON /CHRCOM/ NAMEI, CLASSI, NAMEO, CLASSO
      COMMON /CATHDR/ CATII, CATO, UVMULT, DELTC, DELTF, DOUVMI
C                                       This common MUST match the one
C                                       in DUVH.INC from FREQ to DEQUI
      COMMON /UVHINC/
     *   FREQ1, RA1, DEC1, NVIS1, I1LOCU, I1LOCV, I1LOCW, I1LOCT,
     *   I1LOCB, I1LOCS, I1LOCQ, I1LOCI, I1LOCC, I1LOCA, I1LOC1, I1LOC2,
     *   J1LOCC, J1LOCS, J1LOCF, J1LOCR, J1LOCD, J1LOCI, INCS1, INCF1,
     *   INCIF1, ICOR01, NPARM1, LREC1, NCOR1, TYUVD1, DEQUI1,
     *   FREQ2, RA2, DEC2, NVIS2, I2LOCU, I2LOCV, I2LOCW,I2LOCT, I2LOCB,
     *   I2LOCS, I2LOCQ, I2LOCI, I2LOCC, I2LOCA, I2LOC1, I2LOC2,
     *   J2LOCC, J2LOCS, J2LOCF, J2LOCR, J2LOCD, J2LOCI, INCS2, INCF2,
     *   INCIF2, ICOR02, NPARM2, LREC2, NCOR2, TYUVD2, DEQUI2
C                                                          End DBAPP
LOCAL END
      PROGRAM DBAPP
C-----------------------------------------------------------------------
C! Appends uv data files to the output file
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2014-2015, 2018
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
C   Task to concatenate two uv data bases. Output data will be in a
C   format which will hold both data sets but antenna files will be
C   separatly copied.  No attempt will be made to renumber antennas.
C   Sources will be renumbered as necessary.
C   Inputs:
C      Adverb      Pgm. name      Description
C      INNAME      NAMEI          First uv file name
C      INCLASS     CLASSI         First uv file class
C      INSEQ       SEQ1           First uv file sequence no.
C      INDISK      DISKI          First uv file disk number.
C      IN2SEQ      SEQ2           Second uv file sequence no.
C      OUTNAME     NAME3          Output uv file name
C      OUTCLASS    CLASS3         Output uv file class
C      OUTSEQ      SEQ3           Output uv file sequence no.
C      OUTDISK     DISK3          Output uv file disk no.
C      DOPOS       DOPOS          First value (1,1):
C                                 If <= 0 do not check positions.
C                                 If > 0 will shift position of second
C                                 data set to that of first.
C                                 Second value (2,1):
C                                 If <= 0 check frequency for multi-
C                                 channel data sets.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR, II
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DBAPP.INC'
      DATA PRGM /'DBAPP '/
C-----------------------------------------------------------------------
C                                       Get inputs.
      IRET = 8
      CALL DBAPIN (PRGM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Loop over inputs
      DO 100 II = 2,NCFILE
         DISKI = FVOL(II)
         CNOI  = FCNO(II)
         CALL CATIO ('READ', DISKI, CNOI, CATII, 'REST', BUFF1, IERR)
         IF (IERR.GT.1) THEN
            WRITE (MSGTXT,1000) IERR, DISKI, CNOI
            CALL MSGWRT (8)
            GO TO 990
            END IF
         WRITE (MSGTXT,1010) II-1, DISKI, CNOI
         CALL MSGWRT (2)
C                                       Check data compatibility and
C                                       determine output format.
         CALL FRMAT (IERR)
         IF (IERR.NE.0) GO TO 990
C                                       Merge source tables.
         IF (MULTI) THEN
            CALL DBSOUR (IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Merge AN tables without
C                                       renumbering antennas.
         IF (FIXANT) THEN
            CALL DBANT (IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
C                                       Sequential copy
         CALL DBCOPY (IERR)
         IF (IERR.NE.0) GO TO 990
C                                       history, tables
         CALL HISANT
 100     CONTINUE
      IRET = 0
C                                       Close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('COULD NOT READ INPUT CATALOG HEADER: ERROR, DISK, CNO',
     *   I3,I3,I7)
 1010 FORMAT ('Processing file',I3,' disk',I3,' cno',I6)
      END
      SUBROUTINE DBAPIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   DBAPIN gets inputs for DBAPP and marks input files 'READ'
C   Inputs:
C      PRGM   C*6   Task name
C   Output:
C      IRET   I     Return error code, 0 = OK,
C                                      1 = Error, abort.
C                                      2 = only 1 multisource
C-----------------------------------------------------------------------
      CHARACTER PRGM*(*)
      INTEGER   IRET
C
      CHARACTER STAT*4, UTYPE*2, CHDATE*8, PNAME*48, KEYWRD*8
      INTEGER   CNO, IROUND, NPARM, IERR, LUN, DISK, SEQ, N, INSIZE,
     *   OUTSIZ, FIND, CORCOI, CORCOO, ARRAY1(1), ARRAY2(1), ARRAY3(1),
     *   NUMKEY, LOCS, VALUE, KEYTYP, NWORDS, SAVDSK
      LOGICAL   T, F, TABLE, FITASC, ISOMS, ISMS, FIRST
      DOUBLE PRECISION JD1, JDO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DBAPP.INC'
      EQUIVALENCE (ARRAY1, FREQ1),  (ARRAY2, FREQ2),  (ARRAY3, FREQ)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /27/
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                        Initialize
      IRET = 0
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      BUFSZ = UVBFSL * 2
      FIXANT = .FALSE.
C                                        Get input.
      NPARM = 16
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
         IRET = 8
         RQUICK = F
         GO TO 990
         END IF
C                                        Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
C                                        Crunch input.
      SEQ1 = IROUND (XS1)
      SEQ2 = IROUND (XS2)
      SEQ2 = MAX (SEQ1, SEQ2)
      SEQO = IROUND (XSO)
      DISKI = IROUND (XD1)
      DISKO = IROUND (XDO)
      CALL FILL (10, 0, IBAD)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAME1, NAMEI)
      CALL H2CHR (6, 1, XCLAS1, CLASSI)
      CALL H2CHR (12, 1, XNAMEO, NAMEO)
      CALL H2CHR (6, 1, XCLASO, CLASSO)
      IF (XFQTOL.LE.0.0) XFQTOL = 100.0
      XFQTOL = XFQTOL * 1000.0
C                                        Find output file and get CAT
      CNOO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKO, CNOO, NAMEO, CLASSO, SEQO, UTYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, NAMEO, CLASSO, SEQO, 'UV', DISKO,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKO, CNOO, CATO, 'WRIT', BUFF1, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1000) IRET,
     *      'READING CATALOG HEADER FOR INPUT FILE'
         GO TO 990
         END IF
C                                        Update /CFILE/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 1
C                                       See if multisource
      CALL MULSDB (CATO, ISOMS)
      IF (ISOMS) THEN
         CALL ISTAB ('SU', DISKO, CNOO, 1, LUN, BUFF1, TABLE, ISOMS,
     *      FITASC, IERR)
         ISOMS = ISOMS .AND. (IERR.EQ.0)
         END IF
C                                       reference date
      CALL H2CHR (8, 1, CATHO(KHDOB), CHDATE)
      CALL JULDAY (CHDATE, JDO)
C                                       All data in the same subarray
      DOARR = 1.0
C                                       Do not shift positions
      DOPOS = 0.0
      MULTI = .FALSE.
C                                       vis or corr coeff?
      NUMKEY = 1
      KEYWRD = 'CORRCOEF'
      CALL CATKEY ('REED', DISKO, CNOO, KEYWRD, NUMKEY, LOCS, VALUE,
     *   KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         CORCOO = 0
      ELSE
         CORCOO = VALUE
         END IF
C                                       pointers
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (IERR)
      NNIF = 1
      IF (JLOCIF.GE.0) NNIF = CATBLK(KINAX+JLOCIF)
C                                        Fill values in /CATHDR/
      NWORDS = 28 + 3 * NWDPDP
      CALL COPY (NWORDS, ARRAY3, ARRAY2)
C                                       only do one subarray
      CALL FNDEXT ('AN', CATBLK, NUMKEY)
      IF (NUMKEY.GT.1) THEN
         MSGTXT = 'ONLY WORKS FOR A SINGLE SUBARRAY DATA SET'
         IRET = 10
         GO TO 990
         END IF
C                                       fill DANS for output file
      NUMKEY = 1
      CALL GETANT (DISKO, CNOO, NUMKEY, CATBLK, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'GETTING OUTPUT FILE ANTENNA INFO'
         GO TO 990
         END IF
C                                       Get input files
      NEWVIS = 0
      SAVDSK = DISKI
      DO 100 SEQ = SEQ1,SEQ2
         CNO = 1
         UTYPE = 'UV'
         DISK = SAVDSK
         CALL CATDIR ('SRCH', DISK, CNO, NAMEI, CLASSI, SEQ, UTYPE,
     *      NLUSER, STAT, BUFF1, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, NAMEI, CLASSI, SEQ, 'UV', DISK,
     *         NLUSER
            GO TO 990
            END IF
         CALL CATIO ('READ', DISK, CNO, CATII, 'READ', BUFF1, IRET)
         IF (IRET.GT.1) THEN
            WRITE (MSGTXT,1000) IRET,
     *         'READING CATALOG HEADER FOR INPUT FILE'
            GO TO 980
            END IF
C                                        Update /CFILE/
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISK
         FCNO(NCFILE) = CNO
         FRW(NCFILE) = 0
         NEWVIS = NEWVIS + CATII(KIGCN)
         CNOI = CNO
         DISKI = DISK
C                                       See if multisource
         CALL MULSDB (CATII, ISMS)
         IF (ISMS) THEN
            CALL ISTAB ('SU', DISKI, CNOI, 1, LUN, BUFF1, TABLE, ISMS,
     *         FITASC, IERR)
            ISMS = ISMS .AND. (IERR.EQ.0)
            END IF
C                                       Both multi-source
         IF ((ISOMS) .AND. (ISMS)) THEN
            MULTI = .TRUE.
C                                       Can't combine
         ELSE IF ((ISOMS) .OR. (ISMS)) THEN
            IRET = 2
            MSGTXT = 'CANNOT COMBINE SINGLE AND MULTISOURCE FILES'
            GO TO 980
            END IF
C                                       cannot change reference day
         CALL H2CHR (8, 1, CATHI(KHDOB), CHDATE)
         CALL JULDAY (CHDATE, JD1)
         IF (JD1.LT.JDO) THEN
            MSGTXT = 'REFERENCE DATE OF OUTPUT MUST PRECEDE ALL INPUTS'
            IRET = 2
            GO TO 980
            END IF
C                                       get header keyword CORRCOEF
         NUMKEY = 1
         CALL CATKEY ('REED', DISKI, CNOI, KEYWRD, NUMKEY, LOCS, VALUE,
     *      KEYTYP, BUFF1, IERR)
         IF (IERR.NE.0) THEN
            CORCOI = 0
         ELSE
            CORCOI = VALUE
            END IF
         IF (CORCOI.NE.CORCOO) THEN
            IF ((CORCOI.NE.0) .AND. (CORCOO.NE.0)) THEN
               MSGTXT = 'CANNOT COMBINE CORRELATION COEFFICIENTS WITH'
     *            // ' VISIBILITIES'
               GO TO 990
            ELSE IF ((CORCOI.EQ.1) .OR. (CORCOO.EQ.1)) THEN
               MSGTXT = 'WARNING: COMBINING CORR. COEFFICIENTS WITH'
     *            // 'DATA OF UNKNOWN TYPE'
               CALL MSGWRT (7)
            ELSE
               MSGTXT = 'WARNING: COMBINING VISIBILITY DATA WITH DATA'
     *            // 'OF UNKNOWN TYPE'
               CALL MSGWRT (7)
               END IF
            END IF
C                                       dimensionality
         IF (CATII(KIDIM).NE.CATO(KIDIM)) THEN
            MSGTXT = 'NUMBER OF REGULAR AXES DOES NOT MATCH'
            GO TO 980
            END IF
         IF (CATII(KIPCN).NE.CATO(KIPCN)) THEN
            MSGTXT = 'NUMBER OF RANDOM PARAMETERS DOES NOT MATCH'
            GO TO 980
            END IF
C                                       pointers
         CALL COPY (256, CATII, CATBLK)
         CALL UVPGET (IERR)
C                                        Fill values in /CATHDR/
         NWORDS = 28 + 3 * NWDPDP
         CALL COPY (NWORDS, ARRAY3, ARRAY1)
C                                       array size
         IF ((CATII(KINAX+J1LOCS).NE.CATO(KINAX+J2LOCS)) .OR.
     *      (CATII(KINAX+J1LOCF).NE.CATO(KINAX+J2LOCF)) .OR.
     *      (CATII(KINAX+J1LOCI).NE.CATO(KINAX+J2LOCI))) THEN
            MSGTXT = 'NUMBERS OF POINTS ON AXES DO NOT MATCH'
            GO TO 980
            END IF
C                                       check axis information
         CALL CHKAX (IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR COMPARING INPUT AND OUTPUT AXIS INFO'
            GO TO 980
            END IF
C                                       check antenna information
         CALL CHKANT (IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR COMPARING INPUT AND OUTPUT ANTENNA TABLES'
            GO TO 980
            END IF
C                                       check frequency information
         CALL CHKFQ (FIRST, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR COMPARING INPUT AND OUTPUT FREQ TABLES'
            GO TO 980
            END IF
         FIRST = .FALSE.
C                                       Check compatibility
         IF ((.NOT.MULTI) .AND. (TYPUVD.LE.0)) THEN
C                                       Check Epoch
            IF (CATRI(KREPO).NE.CATRO(KREPO)) THEN
               WRITE (MSGTXT,1100) SEQ, CATRI(KREPO), CATRO(KREPO)
               CALL MSGWRT (7)
               END IF
C                                        Check RA
            IF (ABS(RA1-RA2).GT.3.0E-10) THEN
               WRITE (MSGTXT,1101) SEQ, 'RA'
               CALL MSGWRT (7)
               END IF
C                                        Check Dec.
            IF (ABS(DEC1-DEC2).GT.3.0E-10) THEN
               WRITE (MSGTXT,1101) SEQ, 'DEC'
               CALL MSGWRT (7)
               END IF
            END IF
 100     CONTINUE
C                                       expand the output file
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, PNAME, IERR)
      N = CATO(KIGCN)
      CALL UVSIZE (LREC, N, INSIZE)
      N = N + NEWVIS
      CALL UVSIZE (LREC, N, OUTSIZ)
      N = OUTSIZ - INSIZE
      CALL ZOPEN (LUN, FIND, DISKO, PNAME, .TRUE., .TRUE., .TRUE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT FILE TO EXPAND IT'
         GO TO 990
         END IF
      CALL ZEXPND (LUN, DISKO, PNAME, N, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'EXPANDING OUTPUT FILE'
         GO TO 990
         END IF
      CALL ZCLOSE (LUN, FIND, IERR)
      IRET = 0
      GO TO 999
C                                       error inside loop
 980  CALL MSGWRT (8)
      IF (IRET.LE.0) IRET = 8
      WRITE (MSGTXT,1980) SEQ
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I5,1X,A)
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I2,' USER=',I4)
 1100 FORMAT ('SEQ',I6,' EPOCHS DO NOT MATCH',2F8.1)
 1101 FORMAT ('SEQ',I6,1X,A,' DOES NOT MATCH')
 1980 FORMAT ('ERROR AROSE WITH INPUT FILE SEQUENCE NUMBER',I6)
      END
      SUBROUTINE CHKAX (IRET)
C-----------------------------------------------------------------------
C   Compares axis information to make sure all is well
C   Output:
C      IRET   I    0 => match okay, 1 => they don't
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J, M, N
      CHARACTER CHTEMP*8, OUTHDR(14)*8
      INCLUDE 'DBAPP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IRET = 0
      N = CATO(KIDIM)
      IF (CATII(KIDIM).NE.N) THEN
         WRITE (MSGTXT,1000) N, CATII(KIDIM)
         CALL MSGWRT (7)
         IRET = IRET + 1
         END IF
      M = KHCTP
      DO 10 I = 1,N
         CALL H2CHR (8, 1, CATHO(M), OUTHDR(I))
         M = M + 2
 10      CONTINUE
      M = KHCTP
      DO 20 J = 1,CATII(KIDIM)
         CALL H2CHR (8, 1, CATHI(M), CHTEMP)
         M = M + 2
         DO 15 I = 1,N
            IF (CHTEMP.EQ.OUTHDR(I)) GO TO 20
 15         CONTINUE
         WRITE (MSGTXT,1015) CHTEMP
         CALL MSGWRT (7)
         IRET = IRET + 1
 20      CONTINUE
C                                       random parameters
      N = CATO(KIPCN)
      IF (CATII(KIPCN).NE.N) THEN
         WRITE (MSGTXT,1020) N, CATII(KIPCN)
         CALL MSGWRT (7)
         IRET = IRET + 1
         END IF
      M = KHPTP
      DO 30 I = 1,N
         CALL H2CHR (8, 1, CATHO(M), OUTHDR(I))
         M = M + 2
 30      CONTINUE
      M = KHPTP
      DO 40 J = 1,CATII(KIPCN)
         CALL H2CHR (8, 1, CATHI(M), CHTEMP)
         M = M + 2
         DO 35 I = 1,N
            IF (CHTEMP.EQ.OUTHDR(I)) GO TO 40
 35         CONTINUE
         WRITE (MSGTXT,1035) CHTEMP
         CALL MSGWRT (7)
         IRET = IRET + 1
 40      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NUMBER OF AXES DISAGREE',2I4)
 1015 FORMAT ('AXIS ''',A,''' NOT FOUND IN OUTPUT')
 1020 FORMAT ('NUMBER OF RANDOM PARAMETERS DISAGREE',2I4)
 1035 FORMAT ('RANDOM PARAMETER ''',A,''' NOT FOUND IN OUTPUT')
      END
      SUBROUTINE CHKANT (IRET)
C-----------------------------------------------------------------------
C   CHKANT makes sure that the antenna file for DISKI, CNOI matches that
C   for the output file (already in DANS.INC)
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'DBAPP.INC'
      INTEGER   LUN, NR, IR, IVER
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUN /40/
C-----------------------------------------------------------------------
      IVER = 1
      CALL ANTINI ('READ', BUFF1, DISKI, CNOI, IVER, CATII, LUN, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      NR = TBUFF1(5)
      IF ((ABS(CNTRX-ARRAYC(1)).GT.1.) .OR. (ABS(CNTRY-ARRAYC(2)).GT.1.)
     *   .OR. (ABS(CNTRZ-ARRAYC(3)).GT.1.) .OR. (TIMLAB.NE.TIMSYS) .OR.
     *   (DATUTC.NE.ANTIAT)) THEN
         IRET = 7
         MSGTXT = 'ANTENNA TABLE HEADERS DO NOT MATCH'
         GO TO 990
         END IF
      IF (UT1UTC.NE.ANTUTC) THEN
         MSGTXT = 'UT1UTC PARAMETER IN ANTENNA FILES DIFFERS'
         CALL MSGWRT (7)
         END IF
C                                      Read AN records
      DO 200 IR = 1,NR
         IANRNO = IR
         CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT FILE ANTENNA TABLE'
            GO TO 900
            END IF
C                                       Skip records with
C                                       zero station numbers
         IF ((NOSTA.LE.0) .OR. (NOSTA.GT.MAXANT)) THEN
            WRITE (MSGTXT,1010) NOSTA, IR, IVER
            CALL MSGWRT (6)
            GO TO 200
            END IF
C
         IF ((STNNAM(NOSTA).NE.' ') .AND. (STNNAM(NOSTA).NE.'OUT') .AND.
     *      (ANNAME.NE.'OUT')) THEN
            IF ((STNNAM(NOSTA).NE.ANNAME) .OR. (TELNO(NOSTA).NE.NOSTA)
     *         .OR. (ABS(STNX(NOSTA)-STAXYZ(1)).GT.2.)
     *         .OR. (ABS(STNY(NOSTA)-STAXYZ(2)).GT.2.)
     *         .OR. (ABS(STNZ(NOSTA)-STAXYZ(3)).GT.2.)) THEN
               WRITE (MSGTXT,1020) NOSTA
               IRET = 8
               GO TO 900
               END IF
         ELSE IF (ANNAME.NE.'OUT') THEN
            FIXANT = .TRUE.
            END IF
 200     CONTINUE
C                                      Close AN extension file
 900  IF (IRET.GT.0) CALL MSGWRT (8)
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKANT ERROR;',I4,' ON ',A)
 1010 FORMAT ('BAD STATION NUMBER',I6,' FOR ENTRY=',I4,' SUBA=',I3)
 1020 FORMAT ('CHKANT: STATION',I3,' NAME OR POSITION DOES NOT AGREE')
      END
      SUBROUTINE CHKFQ (FIRST, IRET)
C-----------------------------------------------------------------------
C   Compares output and input FQ tables
C   Input:
C      FIRST   L   T => read output table
C   Outputs:
C      IRET    I   Error or does not match
C-----------------------------------------------------------------------
      LOGICAL   FIRST
      INTEGER   IRET
C
      INCLUDE 'DBAPP.INC'
      INTEGER   MAXFQN
      PARAMETER (MAXFQN = 20)
      INTEGER   LUN, KOLS(MAXFQC), NUMV(MAXFQC), NUMIFQ, NR, I, RNOFQ,
     *   FQID, IIF
      DOUBLE PRECISION FREQFQ(MAXIF)
      REAL      TBWFQ(MAXIF), CHBWFQ(MAXIF)
      INTEGER   SIDFQ(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8

      DOUBLE PRECISION FQFRQS(MAXIF,MAXFQN)
      REAL             FQINCS(MAXIF,MAXFQN)
      INTEGER          FQSIDS(MAXIF,MAXFQN)
      COMMON /FQTABL/ FQFRQS, FQINCS, FQSIDS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA LUN /41/
C-----------------------------------------------------------------------
C                                       input freq at ref pix of output
      FREQFQ(1) = CATDI(KDCRV+J1LOCF) + CATRI(KRCIC+J1LOCF) *
     *   (CATRO(KRCRP+J2LOCF) - CATRI(KRCRP+J1LOCF))
      IF (ABS(FREQFQ(1)-CATDO(KDCRV+J2LOCF)).GT.XFQTOL) THEN
         WRITE (MSGTXT,1010) FREQFQ(1)/1.D9, CATDO(KDCRV+J2LOCF)/1.D9
         CALL MSGWRT (7)
         IRET = 9
         GO TO 999
         END IF
C                                       fill comparison table
      IF (FIRST) THEN
         I = MAXIF * MAXFQN
         CALL RFILL (I, 0.0, FQINCS)
         CALL DFILL (I, 0.0D0, FQFRQS)
         CALL FQINI ('READ', TBUFF1, DISKO, CNOO, 1, CATO, LUN, RNOFQ,
     *      KOLS, NUMV, NUMIFQ, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPENING FQ TABLE OF OUTPUT'
            GO TO 990
            END IF
         NR = TBUFF1(5)
         DO 50 I = 1,NR
            RNOFQ = I
            CALL TABFQ ('READ', TBUFF1, RNOFQ, KOLS, NUMV, NUMIFQ, FQID,
     *         FREQFQ, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE OF OUTPUT'
               GO TO 990
               END IF
            IF ((FQID.GE.1) .AND. (FQID.LE.MAXFQN)) THEN
               DO 20 IIF = 1,NUMIFQ
                  FQFRQS(IIF,FQID) = FREQFQ(IIF)
                  FQINCS(IIF,FQID) = CHBWFQ(IIF)
                  FQSIDS(IIF,FQID) = SIDFQ(IIF)
 20               CONTINUE
               END IF
 50         CONTINUE
         CALL TABIO ('CLOS', 0, RNOFQ, TBUFF1, TBUFF1, I)
         END IF
C                                       now test
      CALL FQINI ('READ', TBUFF1, DISKI, CNOI, 1, CATII, LUN, RNOFQ,
     *   KOLS, NUMV, NUMIFQ, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING FQ TABLE OF INPUT'
         GO TO 990
         END IF
      NR = TBUFF1(5)
      DO 90 I = 1,NR
         RNOFQ = I
         CALL TABFQ ('READ', TBUFF1, RNOFQ, KOLS, NUMV, NUMIFQ, FQID,
     *      FREQFQ, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING FQ TABLE OF INPUT'
            GO TO 990
            END IF
         IF ((FQID.GE.1) .AND. (FQID.LE.MAXFQN)) THEN
            DO 70 IIF = 1,NUMIFQ
               IF (ABS(FQFRQS(IIF,FQID)-FREQFQ(IIF)).GT.XFQTOL)
     *            GO TO 100
               IF (ABS(FQINCS(IIF,FQID)-CHBWFQ(IIF)).GT.XFQTOL/100.)
     *            GO TO 100
               IF (FQSIDS(IIF,FQID).NE.SIDFQ(IIF)) GO TO 100
 70            CONTINUE
            END IF
 90      CONTINUE
      CALL TABIO ('CLOS', 0, RNOFQ, TBUFF1, TBUFF1, I)
      GO TO 999
C                                       failure
 100  CALL TABIO ('CLOS', 0, RNOFQ, TBUFF1, TBUFF1, I)
      WRITE (MSGTXT,1100) IIF, FQID
      IRET = 8
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKFQ ERROR',I4,' ON ',A)
 1010 FORMAT ('HEADER FREQS',2F10.6,' MISMATCH')
 1100 FORMAT ('CHKFQ: MISMATCH AT IF, FQID',2I4)
      END
      SUBROUTINE FRMAT (IRET)
C-----------------------------------------------------------------------
C   FRMAT determines the compatibility of files, determines the output
C   format and the translation tables for the three formats and creates
C   the output file.
C   Output:
C      IRET    I     Return error code, 0 => OK, else abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER SRTYPE(2)*8, CHDATE*8
      INTEGER   ARRAY1(1), ARRAY2(1), ARRAY3(1), IERR, NWORDS, LIM, I,
     *   NRAN, INDEX, LIM2, J, JNDX, JJ
      LOGICAL   EQUAL
      DOUBLE PRECISION JDI, JDO, TIMADD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBAPP.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ARRAY1, FREQ1),  (ARRAY2, FREQ2),  (ARRAY3, FREQ)
C-----------------------------------------------------------------------
      IRET = 1
      ISCMPI = CATII(KINAX).EQ.1
      ISCMPO = CATO(KINAX).EQ.1
C                                       Header #1, leave in CATBLK
      CALL COPY (256, CATII, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      NWORDS = 28 + 3 * NWDPDP
      CALL COPY (NWORDS, ARRAY3, ARRAY1)
C                                       Convert ref. freq.
      DOUVMI = ABS (CATRO(KRCRP+J2LOCF)-CATRI(KRCRP+JLOCF)).GT.0.01
      DOUVMI = DOUVMI .AND. (TYPUVD.LE.0)
      UVMULT = CATDI(KDCRV+JLOCF)
      DELTC = 0.0D0
      DELTF = 0.0D0
      IF (DOUVMI) THEN
         DELTC = (CATRO(KRCRP+J2LOCF) - CATRI(KRCRP+JLOCF))
         DELTF = DELTC * CATRI(KRCIC+JLOCF)
         CATDI(KDCRV+JLOCF) = CATDI(KDCRV+JLOCF) + DELTF
         CATRI(KRCRP+JLOCF) = CATRO(KRCRP+J2LOCF)
         END IF
      CATD(KDCRV+JLOCF) = CATDI(KDCRV+JLOCF)
      CATR(KRCRP+JLOCF) = CATRI(KRCRP+JLOCF)
      FREQ = CATDI(KDCRV+JLOCF)
      UVMULT = FREQ / UVMULT
      WRITE (MSGTXT,1000) CNOI, UVMULT
      IF (DOUVMI) CALL MSGWRT (6)
C                                        Check random parameters.
C                                        Fill random parm. pointers
      LIM = MAX (NPARM1, NPARM2)
      DO 40 I = 1,LIM
         IORD(I) = I - 1
 40      CONTINUE
      NRAN = NPARM1
C                                        Munge random parameters #2.
      DO 50 I = 1,NPARM2
         INDEX = KHPTP + (I-1)*2
         LIM2 = MIN (KIPTPN, NRAN)
         DO 45 J = 1,LIM2
            JNDX = KHPTP + (J-1)*2
            JJ = J
            CALL CHCOMP (8, 1, CATH(JNDX), 1, CATHO(INDEX), EQUAL)
            IF (EQUAL) IORD(I) = JJ - 1
 45         CONTINUE
 50      CONTINUE
C                                        Update output CATBLK pointers
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Check sort order, if both the
C                                        same leave as is.
      CALL H2CHR (8, 1, CATHO(KHDOB), CHDATE)
      CALL JULDAY (CHDATE, JDO)
      CALL H2CHR (8, 1, CATHI(KHDOB), CHDATE)
      CALL JULDAY (CHDATE, JDI)
      TIMADD = JDI - JDO
      CALL H2CHR (2, 1, CATHO(KITYP), SRTYPE(1))
      CALL H2CHR (2, 1, CATHI(KITYP), SRTYPE(2))
      CALL H2CHR (2, 1, CATH(KITYP), ISORT(1:2))
      IF ((SRTYPE(1).NE.SRTYPE(2)) .OR. (SRTYPE(1)(:1).NE.'T')) THEN
         ISORT = '**'
         END IF
      IF (ISORT(:1).EQ.'T') CALL CHKTIM (TIMADD, ISORT, IERR)
      CALL CHR2H (4, ISORT, 1, CATH(KITYP))
      CALL CHR2H (4, ISORT, 1, CATHO(KITYP))
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Scaling u,v,w of data set',I2,' by',1PE15.7)
      END
      SUBROUTINE CHKTIM (TIMADD, SRTORD, IRET)
C-----------------------------------------------------------------------
C   Checks that the last time of the output is < first time of input
C   In/Out:
C      SRTORD   C*4   Sort order: in 'T?'. out '**' if time not okay
C   Output
C      IRET     I     Disk error: sort to ** if NX missing
C-----------------------------------------------------------------------
      DOUBLE PRECISION TIMADD
      CHARACTER SRTORD*(*)
      INTEGER   IRET
C
      INCLUDE 'DBAPP.INC'
      INTEGER   VER, LUN, INXRNO, NXKOLS(MAXNXC), NXNUMV(MAXNXC), IDS,
     *   SUBA, IVS, IVE, FQID, IERR
      REAL      TIME, DTIME, TOUT, TIN

      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       output NX table
      VER = 1
      LUN = 55
      MSGSUP = 32000
      CALL NDXINI ('READ', TBUFF1, DISKO, CNOO, VER, CATO, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IRET)
      MSGSUP = 0
      IF (IRET.EQ.2) THEN
         IRET = 0
         SRTORD = '**'
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT NX TABLE'
      ELSE
         INXRNO = TBUFF1(5)
         CALL TABNDX ('READ', TBUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDS, SUBA, IVS, IVE, FQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OUTPUT NX TABLE'
         ELSE
            TOUT = TIME + DTIME/2.
            END IF
         CALL TABNDX ('CLOS', TBUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDS, SUBA, IVS, IVE, FQID, IERR)
         END IF
      IF (IRET.NE.0) GO TO 990
C                                       input NX table
      MSGSUP = 32000
      CALL NDXINI ('READ', TBUFF1, DISKi, CNOi, VER, CATii, LUN, INXRNO,
     *   NXKOLS, NXNUMV, IRET)
      MSGSUP = 0
      IF (IRET.EQ.2) THEN
         IRET = 0
         SRTORD = '**'
      ELSE IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN inPUT NX TABLE'
      ELSE
         INXRNO = 1
         CALL TABNDX ('READ', TBUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDS, SUBA, IVS, IVE, FQID, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ INPUT NX TABLE'
         ELSE
            TIN = TIMADD + TIME - DTIME/2.
            END IF
         CALL TABNDX ('CLOS', TBUFF1, INXRNO, NXKOLS, NXNUMV, TIME,
     *      DTIME, IDS, SUBA, IVS, IVE, FQID, IERR)
         END IF
      IF (IRET.NE.0) GO TO 990
      IF (TIN.LT.TOUT) THEN
         SRTORD = '**'
         MSGTXT = 'T* DATA NOT IN TIME ORDER: SORT BECOMES ''**'''
         CALL MSGWRT (6)
         END IF
      GO TO 999
C
 990  SRTORD = '**'
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CHKTIM ERROR',I4,' ON ',A)
      END
      SUBROUTINE DBSOUR (IRET)
C-----------------------------------------------------------------------
C   DBSOUR merges the source table of the input file into the output.
C   Makes a translation table for the source ids of the input file.
C   Output:
C      IRET   I   Return error code  0 => ok, else error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4, CHARS*27
      INTEGER   VER, LUN1, LUN2, SUKOL1(MAXSUC), SUKOL2(MAXSUC),
     *   SUNUM1(MAXSUC), SUNUM2(MAXSUC), NUMFST, NUMIF, IDSOU, QUAL,
     *   IDTEMP, NEXT, IERR, I, LUMIF, NRECIN, INREC, OUTREC, LOOP,
     *   SUFQD1, SUFQD2, II, JJ, NRECO
      LOGICAL   T, F, NEW, MATCH
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, PMRA,
     *   PMDEC, RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'DBAPP.INC'
      INTEGER   SUQUAL(MAXSOU)
      CHARACTER SUNAM(MAXSOU)*16, SUCODE(MAXSOU)*4
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      DOUBLE PRECISION DEPOCH(MAXSOU), DRAEPO(MAXSOU), DCEPO(MAXSOU),
     *   SEPS
      DATA LUN1, LUN2 /27, 28/
      DATA T, F /.TRUE.,.FALSE./
      DATA CHARS /' ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      NUMSOU = 0
      II = 1
      JJ = 1
C                                       EPS for source coord comparison
C                                       is 10mas
      SEPS = 0.01D0 / 3600.0D0
C                                       Open first input table
      MSGSUP = 32000
      IRET = 0
      VER = 1
      CALL SOUINI ('WRIT', TBUFF1, DISKO, CNOO, VER, CATO, LUN1, NUMIF,
     *   VELTYP, VELDEF, SUFQD1, INREC, SUKOL1, SUNUM1, IERR)
      MSGSUP = 0
C                                       If not there - quit
      IF (IERR.EQ.2) GO TO 999
      IF (IERR.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT SU TABLE'
         GO TO 990
         END IF
      LUMIF = NUMIF
      IF (NUMIF.NE.NNIF) THEN
         WRITE (MSGTXT,1010) 'OUTPUT', NUMIF, NNIF
         CALL MSGWRT (8)
         END IF
C                                       Get number of input records.
      NRECIN = TBUFF1(5)
      NRECO = NRECIN
C                                       Copy
      NUMFST = 0
      IRET = 6
      CALL DFILL (MAXSOU, 0.0D0, DRAEPO)
      DO 20 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABSOU ('READ', TBUFF1, INREC, SUKOL1, SUNUM1, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING OUTPUT SU TABLE'
            GO TO 990
            END IF
C                                       Check number of sources
         IF (IDSOU.GT.MAXSOU) THEN
            IRET = 7
            WRITE (MSGTXT,1015) IDSOU, MAXSOU
            GO TO 990
            END IF
C                                       Save name, qual, coordinates
         SUQUAL(IDSOU) = QUAL
         SUNAM(IDSOU) = SOUNAM
         SUCODE(IDSOU) = CALCOD
         DEPOCH(IDSOU) = EPOCH
         DRAEPO(IDSOU) = RAEPO
         DCEPO(IDSOU) = DECEPO
         NUMFST = MAX (NUMFST, IDSOU)
 20      CONTINUE
      OUTREC = NRECIN + 1
C                                       Open second input
      IRET = 0
      VER = 1
      CALL SOUINI ('READ', TBUFF2, DISKI, CNOI, VER, CATII, LUN2, NUMIF,
     *   VELTYP, VELDEF, SUFQD2, INREC, SUKOL2, SUNUM2, IERR)
C                                       If not there - quit
      IF (IERR.EQ.2) GO TO 800
      IF (IERR.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1000) IERR, 'OPEING INPUT SOURCE TABLE'
         GO TO 990
         END IF
      IF (NUMIF.NE.NNIF) THEN
         WRITE (MSGTXT,1010) 'INPUT', NUMIF, NNIF
         CALL MSGWRT (8)
         END IF
      IF (NUMIF.NE.LUMIF) THEN
         WRITE (MSGTXT,1020) LUMIF, NUMIF
         CALL MSGWRT (8)
         END IF
C                                       Get number of input records.
      NRECIN = TBUFF2(5)
      IRET = 6
      NUMSOU = NRECIN
      NEXT = NUMFST + 1
      MATCH = F
      CALL FILL (NUMSOU, NEXT, SOUTRA)
C                                       Copy, accumulating translation
C                                       table.
      DO 100 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABSOU ('READ', TBUFF2, INREC, SUKOL2, SUNUM2, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READING INPUT FILE SU TABLE'
            GO TO 990
            END IF
C                                       Make translation table
         NEW = F
         DO 30 I = 1,NUMFST
            IDTEMP = I
            IF ((QUAL.EQ.SUQUAL(I)) .AND. (SOUNAM.EQ.SUNAM(I)) .AND.
     *         (CALCOD.EQ.SUCODE(I))) THEN
               IF ((ABS(DEPOCH(I)-EPOCH).LT.0.1) .AND.
     *            (ABS(DRAEPO(I)-RAEPO).LE.SEPS) .AND.
     *            (ABS(DCEPO(I)-DECEPO).LE.SEPS)) THEN
                  MATCH = T
                  GO TO 40
               ELSE
                  II = II + 1
                  IF (II.EQ.28) THEN
                     II = 1
                     JJ = JJ + 1
                     END IF
                  WRITE (MSGTXT,1025) SUNAM(I), SUQUAL(I)
                  CALL MSGWRT (6)
                  SOUNAM(15:15) = CHARS(JJ:JJ)
                  SOUNAM(16:16) = CHARS(II:II)
                  WRITE (MSGTXT,1026) SOUNAM
                  CALL MSGWRT (6)
                  END IF
               END IF
 30         CONTINUE
C                                       No match - new source.
         NEW = T
         IDTEMP = NEXT
         NEXT = NEXT + 1
C                                       Record source translation num
 40      SOUTRA(IDSOU) = IDTEMP
         IDSOU = IDTEMP
         NUMSOU = MAX (NUMSOU, IDSOU)
C                                       Check number of sources
         IF (IDSOU.GT.MAXSOU) THEN
            IRET = 7
            WRITE (MSGTXT,1015) IDSOU, MAXSOU
            GO TO 990
            END IF
C                                       Write
         IF (NEW) THEN
            CALL TABSOU ('WRIT', TBUFF1, OUTREC, SUKOL1, SUNUM1, IDSOU,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRITING OUTPUT SU TABLE'
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close first input.
      CALL TABIO ('CLOS', 0, INREC, BUFF2, TBUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING INPUT SU TABLE'
         GO TO 990
         END IF
C                                       Close output table
 800  CALL TABIO ('CLOS', 0, OUTREC, BUFF1, TBUFF1, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'CLOSING OUTPUT SU TABLE'
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BDSOUR ERROR',I5,' ON ',A)
 1010 FORMAT ('DBSOUR: ',A,' SU table NUMIF=',I3,' NOT',I3,' OF DATA')
 1015 FORMAT ('DBSOUR: TOO MANY SOURCES:',I5, '>', I5,
     *   ' INCREASE TABLES')
 1020 FORMAT ('DBSOUR: TWO SU TABLES NUMIF =',2I3,' DIFFER!!!')
 1025 FORMAT ('DBSOUR: Coordinates disagree for ''',A,''' Qual',I5)
 1026 FORMAT ('DBSOUR: Source renamed ''',A,'''')
      END
      SUBROUTINE DBANT (IRET)
C-----------------------------------------------------------------------
C   DBANT will update the output AN table for any antenna that occurs
C   in the input file and not in the AN file.
C   Output:
C      IRET   I   Return error code 0 => ok, else error.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C                                       Modified include DANT
C                                       (2 buffers).
C                                       NOTE: uses PARAMETER in PUVD.INC
C                                       Declarations for ANTINI
      INTEGER   ANKOLS(MAXANC,2), ANNUMV(MAXANC,2), NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IANRNO
      CHARACTER ANAME*8, RDATE*8, TIMSYS*8, XYZHAN*8, TFRAME*8
      REAL      POLRXY(2), UT1UTC, DATUTC
      DOUBLE PRECISION  ARRAYC(3), GSTIA0, DEGPDY, SAFREQ
C                                       Declarations for TABAN
      INTEGER   NOSTA, MNTSTA
      CHARACTER ANNAME*8, POLTYA*2, POLTYB*2
      REAL      STAXOF, DIAMAN, FWHMAN(MAXIF), POLAA, POLCA(2*MAXIF),
     *   POLAB, POLCB(2*MAXIF)
      DOUBLE PRECISION  STAXYZ(3), ORBPRM(6)
C                                       End modified include.
      INCLUDE 'DBAPP.INC'
      INTEGER   ANTREC(MAXANT), LUNI, LUNO, NREC, MREC, K, L, IEXTAN
      DOUBLE PRECISION LX, LY, LZ
      DATA LUNI, LUNO /27, 28/
C----------------------------------------------------------------------
C                                       Read output, find missing ants
      CALL FILL (MAXANT, 0, ANTREC)
      IEXTAN = 1
      CALL ANTINI ('WRIT', TBUFF1, DISKO, CNOO, IEXTAN, CATO, LUNO,
     *   IANRNO, ANKOLS(1,1), ANNUMV(1,1), ARRAYC, GSTIA0, DEGPDY,
     *   SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *   TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT AN FILE'
         GO TO 990
         END IF
      NREC = TBUFF1(5)
C                                       Compile the AN index.
      DO 20 K = 1,NREC
         IANRNO = K
         CALL TABAN ('READ', TBUFF1, IANRNO, ANKOLS(1,1), ANNUMV(1,1),
     *      ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *      FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING OUTPUT AN FILE'
            GO TO 990
            END IF
         LX = ARRAYC(1) + STAXYZ(1)
         LY = ARRAYC(2) + STAXYZ(2)
         LZ = ARRAYC(3) + STAXYZ(3)
         LX = LX*LX + LY*LY + LZ*LZ
         IF ((NOSTA.GT.0) .AND. (NOSTA.LE.MAXANT)) THEN
            IF ((LX.LE.0.0D0) .OR. (ANNAME.EQ.' ') .OR.
     *         (ANNAME.EQ.'OUT')) THEN
               ANTREC(NOSTA) = -K
            ELSE
               ANTREC(NOSTA) = K
               END IF
            END IF
 20      CONTINUE
C                                       read input file
      CALL ANTINI ('READ', TBUFF2, DISKI, CNOI, IEXTAN, CATII, LUNI,
     *   IANRNO, ANKOLS(1,2), ANNUMV(1,2), ARRAYC, GSTIA0, DEGPDY,
     *   SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN,
     *   TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT AN FILE'
         GO TO 990
         END IF
      MREC = TBUFF2(5)
      DO 50 L = 1,MREC
         IANRNO = L
         CALL TABAN ('READ', TBUFF2, IANRNO, ANKOLS(1,2), ANNUMV(1,2),
     *      ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *      FWHMAN, POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING INPUT AN FILE'
            GO TO 990
            END IF
         LX = ARRAYC(1) + STAXYZ(1)
         LY = ARRAYC(2) + STAXYZ(2)
         LZ = ARRAYC(3) + STAXYZ(3)
         LX = LX*LX + LY*LY + LZ*LZ
         IF ((NOSTA.GT.0) .AND. (NOSTA.LE.MAXANT)) THEN
            IF ((LX.GT.0.0D0) .AND. (ANNAME.NE.' ') .AND.
     *         (ANNAME.NE.'OUT') .AND. (ANTREC(NOSTA).LE.0)) THEN
               IF (ANTREC(NOSTA).EQ.0) THEN
                  NREC = NREC + 1
                  K = NREC
               ELSE
                  K = -ANTREC(NOSTA)
                  END IF
               IANRNO = K
               CALL TABAN ('WRIT', TBUFF1, IANRNO, ANKOLS(1,1),
     *            ANNUMV(1,1), ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA,
     *            STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA, POLCA, POLTYB,
     *            POLAB, POLCB, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT AN FILE'
                  GO TO 990
                  END IF
               END IF
            END IF
 50      CONTINUE
C                                       close up
      CALL TABIO ('CLOS', 0, IANRNO, TBUFF1, TBUFF1, IRET)
      CALL TABIO ('CLOS', 0, IANRNO, TBUFF2, TBUFF2, IRET)
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DBANT ERROR',I4,' ON ',A)
      END
      SUBROUTINE DBCOPY (IRET)
C-----------------------------------------------------------------------
C   DBCOPY copies and reformats the two old data bases into a new
C   database.
C   Output: IRET   I    Return error code, 0 => OK, 1 => error, abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'DBAPP.INC'
      INTEGER   LUNI, LUNO, FINDI, FINDO, IPTRI, IPTRO, IOP, INIO,
     *   NIOUT, BINDI, BINDO, WTOFF, OCNT, I, ILENBU, LLREC, NPARM, IR,
     *   IRN, VO, BO, SUID, NCMPLX, IROUND, NNCOR, WWTOFF, NCH, NST,
     *   NIF, IIF, ICH, IST, NNCORO, XLREC, RNXRET, OREC, INCSI, INCFI,
     *   INCIFI, INCSO, INCFO, INCIFO, INDXI, INDXO, IMXS, MSGSAV, NSUB
      LOGICAL   T, F
      REAL      TIMADD
      DOUBLE PRECISION JDO, JDI
      CHARACTER IFILE*48, OFILE*48, CHDATE*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DRNX.INC'
      LOGICAL SOUCHN(MAXSOU)
      SAVE SOUCHN
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNI, LUNO /16, 17/
      DATA BO /1/
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
C                                       Open first input file
      CALL ZPHFIL ('UV', DISKI, CNOI, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, FINDI, DISKI, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING INPUT UV DATA'
         GO TO 990
         END IF
C                                       Open output file.
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, FINDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT UV DATA'
         GO TO 990
         END IF
C                                       Reset output CATBLK pointers
      CALL COPY (256, CATO, CATBLK)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      NCMPLX = CATBLK(KINAX)
      NNCORO = (LREC - NRPARM) / NCMPLX
      XLREC = NRPARM + 3 * NNCORO
C                                       Find weight for compressed data.
      IF (ISCMPO) CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN),
     *   CATH(KHPTP), WTOFF, IRET)
      IF (ISCMPI) CALL AXEFND (8, 'WEIGHT  ', CATII(KIPCN),
     *   CATII(KHPTP), WWTOFF, IRET)
C                                       Determine read/write sizes.
C                                       Init I/O
      ILENBU = 0
      VO = 0
      CALL UVINIT ('READ', LUNI, FINDI, NVIS1, VO, LREC1, ILENBU,
     *   BUFSZ, BUFF1, BO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING INPUT UV DATA'
         GO TO 990
         END IF
C                                       output
      ILENBU = 0
      VO = NVIS
      CALL UVINIT ('WRIT', LUNO, FINDO, NVIS1, VO, LREC, ILENBU, BUFSZ,
     *   BUFF2, BO, BINDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING OUTPUT UV DATA'
         GO TO 990
         END IF
      IPTRO = BINDO
      NIOUT = ILENBU
      OREC = NVIS
      CALL H2CHR (8, 1, CATHO(KHDOB), CHDATE)
      CALL JULDAY (CHDATE, JDO)
      CALL H2CHR (8, 1, CATHI(KHDOB), CHDATE)
      CALL JULDAY (CHDATE, JDI)
      TIMADD = JDI - JDO
      DAYOFF = TIMADD
C                                       get input NX table info
      CALL RNXGET (DISKI, CNOI, CATII)
      IF (DAYOFF.GT.0) THEN
         CALL FNDEXT ('AN', CATBLK, NSUB)
         IR = 0
         DO 15 I = 1,NSUB
            IR = IR + RNXNOS(I)
 15         CONTINUE
         DO 20 I = 1,IR
            RNXTSC(I) = RNXTSC(I) + DAYOFF
 20         CONTINUE
         END IF
C                                       make an index table
      MSGSUP = 32000
      CALL RNXINI (DISKO, CNOO, CATBLK, RNXRET)
      MSGSUP = MSGSAV
      RNXVIS = OREC
C                                       # channels, Stokes, IFs, in
C                                       second (to be shifted) stream.
      IF (JLOCF.EQ.-1) THEN
         NCH = 1
      ELSE
         NCH = CATBLK(KINAX+JLOCF)
         END IF
      IF (JLOCS.EQ.-1) THEN
         NST = 1
      ELSE
         NST = CATBLK(KINAX+JLOCS)
         END IF
      IF (JLOCIF.EQ.-1) THEN
         NIF = 1
      ELSE
         NIF = CATBLK(KINAX+JLOCIF)
         END IF
      IF (ISCMPO) THEN
         I = 3
      ELSE
         I = 1
         END IF
      INCSO = I * INCS2
      INCFO = I * INCF2
      INCIFO = I * INCIF2
      IF (ISCMPI) THEN
         I = 3
      ELSE
         I = 1
         END IF
      INCSI = I * INCS1
      INCFI = I * INCF1
      INCIFI = I * INCIF1
      OCNT = 0
      LLREC = LREC1
      NPARM = NPARM1
      NCMPLX = CATII(KINAX)
      NNCOR = (LLREC - NPARM) / NCMPLX
C
      IMXS = MAXSOU
      DO 30 I = 1,IMXS
         SOUCHN(I) = .FALSE.
 30      CONTINUE
C                                       Copy file # 1
C                                       Begin loop
 50   CALL UVDISK ('READ', LUNI, FINDI, BUFF1, INIO, BINDI, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ'
         GO TO 990
         END IF
      IF (INIO.GT.0) THEN
         IPTRI = BINDI
C                                       Loop thru records.
         DO 100 IR = 1,INIO
C                                       Copy record.
            IF (ISCMPI) THEN
               CALL RCOPY (NPARM, BUFF1(IPTRI), TBUFF)
               CALL ZUVXPN  (NNCOR, BUFF1(IPTRI+NPARM),
     *            BUFF1(IPTRI+WWTOFF), TBUFF(1+NPARM))
            ELSE
               CALL RCOPY (LLREC, BUFF1(IPTRI), TBUFF)
               END IF
C                                       Translate Source number
            IF (NUMSOU.GT.0) THEN
               SUID = IROUND (TBUFF(1+I1LOCS))
C                                       If id is actually different
               IF ((SUID.GT.0) .AND. (SUID.LE.MAXSOU)) THEN
C                                       Change the Source ID
                  IF (SUID.NE.SOUTRA(SUID)) THEN
                     TBUFF(1+I1LOCS) = SOUTRA(SUID)
                     IF (.NOT.SOUCHN(SUID)) THEN
                        WRITE (MSGTXT,1050) SUID, SOUTRA(SUID)
                        CALL MSGWRT (3)
                        SOUCHN(SUID) = .TRUE.
                        END IF
                     END IF
                  END IF
               END IF
C                                       Scale to ref. pixel 1
            IF (DOUVMI) THEN
               TBUFF(1+I1LOCU) = UVMULT * TBUFF(1+I1LOCU)
               TBUFF(1+I1LOCV) = UVMULT * TBUFF(1+I1LOCV)
               TBUFF(1+I1LOCW) = UVMULT * TBUFF(1+I1LOCW)
               END IF
C                                       Zero output record.
            CALL RFILL (XLREC, 0.0, XBUFF)
C                                       Copy random parameters
            DO 70 IRN = 1,NPARM
               IOP = IORD(IRN)
               XBUFF(1+IOP) = TBUFF(IRN)
 70            CONTINUE
C                                        Adjust time
            XBUFF(1+ILOCT) = XBUFF(1+ILOCT) + TIMADD
C                                       Copy data.
            DO 85 IIF = 1,NIF
               DO 80 ICH = 1,NCH
                  DO 75 IST = 1,NST
                     INDXI = (IST-1) * INCSI + (ICH-1) * INCFI +
     *                  (IIF - 1) * INCIFI
                     INDXO = (IST-1) * INCSO + (ICH-1) * INCFO +
     *                  (IIF - 1) * INCIFO
                     INDXI = INDXI + NPARM
                     INDXO = INDXO + NRPARM
                     XBUFF(INDXO+1) = TBUFF(INDXI+1)
                     XBUFF(INDXO+2) = TBUFF(INDXI+2)
                     XBUFF(INDXO+3) = TBUFF(INDXI+3)
 75                  CONTINUE
 80               CONTINUE
 85            CONTINUE
C                                       update NX table
            CALL RNXUPD (XBUFF, RNXRET)
            OREC = OREC + 1
            IF (MOD(OREC,10000).EQ.0) THEN
               WRITE (MSGTXT,1080) OREC
               CALL MSGWRT (2)
               END IF
C                                       move to output buffer
            IF (ISCMPO) THEN
               CALL RCOPY (NRPARM, XBUFF, BUFF2(IPTRO))
               CALL ZUVPAK (NNCORO, XBUFF(1+NRPARM), BUFF2(IPTRO+WTOFF),
     *            BUFF2(IPTRO+NRPARM))
            ELSE
               CALL RCOPY (LREC, XBUFF, BUFF2(IPTRO))
               END IF
C                                       Update pointers.
            OCNT = OCNT + 1
            IPTRI = IPTRI + LLREC
            IPTRO = IPTRO + LREC
C                                       Write if necessary
            IF (OCNT.GE.NIOUT) THEN
               NIOUT = OCNT
               CALL UVDISK ('WRIT', LUNO, FINDO, BUFF2, NIOUT, BINDO,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT UV DATA'
                  GO TO 990
                  END IF
               OCNT = 0
               IPTRO = BINDO
               END IF
 100        CONTINUE
C                                       Loop back
         GO TO 50
         END IF
C                                       Close input
      CALL ZCLOSE (LUNI, FINDI, IRET)
C                                       Flush and close files.
      NIOUT = -OCNT
      CALL UVDISK ('FLSH', LUNO, FINDO, BUFF2, NIOUT, BINDO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSHING OUTPUT UV DATA'
         GO TO 990
         END IF
C                                       Close
      CALL ZCLOSE (LUNO, FINDO, IRET)
      IRET = 0
      NVIS2 = NVIS2 + NVIS1
      CATO(KIGCN) = CATO(KIGCN) + NVIS1
      CATBLK(KIGCN) = CATO(KIGCN)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I4,' ON ',A)
 1050 FORMAT ('DBCOPY: changing source ID',I5,' to',I5)
 1080 FORMAT ('DBCOPY: at output record',I10)
      END
      SUBROUTINE HISANT
C-----------------------------------------------------------------------
C   HISANT updates history files when FILENO is 2 and appends some of
C   tables
C-----------------------------------------------------------------------
      INTEGER   NUMAPP
      PARAMETER (NUMAPP=13)
C
      INCLUDE 'DBAPP.INC'
      CHARACTER HILINE*72, APTYPE(NUMAPP)*2
      INTEGER   LUN1, LUN2, IVER, OVER, IERR, I, NVER1, NVER2, NVER
      REAL      TIMADD
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA APTYPE /'CL','FG','TY','WX','IM','MC','PC','AT','CT','GC',
     *   'OB','SY','SN'/
C-----------------------------------------------------------------------
C                                       Write History.
C
      CALL HIINIT (2)
C                                       open output HI file
      CALL HIOPEN (LUN1, DISKO, CNOO, BUFF2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPENING OUTPUT HISTORY FILE'
         CALL MSGWRT (7)
C                                       New history
      ELSE
         CALL HENCO1 (TSKNAM, NAMEI, CLASSI, SEQ1, DISKI, LUN1, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (HILINE,1020) TSKNAM, SEQ2
         CALL HIADD (LUN1, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (HILINE,1025) TSKNAM, XFQTOL/1.E3
         CALL HIADD (LUN1, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       Close history
 90      CALL HICLOS (LUN1, .TRUE., BUFF2, IERR)
         END IF
C                                       Append table(s)
      DO 200 I = 1,NUMAPP
         CALL FNDEXT (APTYPE(I), CATII, NVER1)
         CALL FNDEXT (APTYPE(I), CATO, NVER2)
         NVER = MAX (NVER1, NVER2)
         IF (NVER.GT.1) THEN
            MSGTXT = 'WARNING: APPENDING MULTIPLE VERSIONS OF ' //
     *         APTYPE(I) // ' TABLES'
            CALL MSGWRT (7)
            MSGTXT = '         THIS MAY NOT BE DESIRABLE'
            CALL MSGWRT (7)
            END IF
         DO 150 IVER = 1,NVER
            OVER = IVER
            TIMADD = DAYOFF
C                                       Update times
            CALL DBCAPP (APTYPE(I), IVER, OVER, LUN1, LUN2, DISKI,
     *         DISKO, CNOI, CNOO, CATBLK, NUMSOU, SOUTRA, DAYOFF,
     *         .FALSE., IBUFF1, IBUFF2, IERR)
 150        CONTINUE
 200     CONTINUE
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', BUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HISANT: ERROR',I3,' ON ',A)
 1020 FORMAT (A6,'IN2SEQ =',I6,'  / append SEQ = INSEQ1:INSEQ2')
 1025 FORMAT (A6,'FQTOL =',F9.3,'  / kHz frequency tolerance')
      END
      SUBROUTINE DBCAPP (TYPE, INVER, OUTVER, LUN1, LUN2, VOL1, VOL2,
     *   CNO1, CNO2, CATBLK, NUMSOU, SOUTRA, TOFF, REWRIT, BUFF1, BUFF2,
     *   IRET)
C-----------------------------------------------------------------------
C   DBCAPP appends one table (input) onto the end of a similar table
C   (output) with the translation of source id numbers.
C   Inputs:
C    TYPE       C*2   Extension table type (e.g. 'CC','AN')
C    INVER      I     Version number to append, 0 => highest.
C    OUTVER     I     Version number on output file, 0=>create
C                     a new one.
C    LUN1       I     LUN for first file (input)
C    LUN2       I     LUN for second file (output)
C    VOL1       I     Disk number for first file.
C    VOL2       I     Disk number for second file.
C    CNO1       I     Catalog slot number for first file
C    CNO2       I     Catalog slot number for second file
C    NUMSOU     I     Number of entries in translation table
C    SOUTRA(*)  I     Source id translation table.
C    TOFF       R     Time offset in days
C    REWRIT     L     If TRUE start writing output at begenning of the
C                     output table
C   In/out:
C    CATBLK(256)I     Catalog header for the first file.
C   Output:
C    BUFF1(512) I     Work buffer
C    BUFF2(512) I     Work buffer
C    IRET       I     Return error code  0 => ok
C                                        1 => files the same, no copy.
C                                        2 => no input files exist
C                                        3 => failed
C                                        4 => no output files created.
C                                        5 => failed to update CATBLK
C-----------------------------------------------------------------------
      CHARACTER TYPE*2
      INTEGER   INVER, OUTVER, LUN1, LUN2, VOL1, VOL2, CNO1, CNO2,
     *   CATBLK(256), NUMSOU, SOUTRA(*), BUFF1(*), BUFF2(*), IRET
      REAL      TOFF
      LOGICAL   REWRIT
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REST*4, SIDKEY(7)*24
      INTEGER   NOTIME, IT2, IT, CATTMP(256)
      REAL      RECR(XBPRSZ)
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INTEGER   NKEY, NREC, NCOL, DATP(128,2,2), IER, I, SOUKOL, LOOP,
     *   RECORD(XBPRSZ), SUID, NRECIN, NRECOU, INREC, OUTREC, TIMKOL
      LOGICAL   T, F, NEW, GOTSOU, NEWSID, DOTIME, DOTIMR, DOTIMD,
     *   TABLE, EXIST, FITASC, OPEN1, OPEN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RECORD, RECR, RECD, DATP)
      DATA REST /'REST'/
      DATA SIDKEY /'SOURCE_ID ', 'SOURCE  ', 'TIME ', 'TIME RANGE ',
     *   'FREQ ID', 'SUBARRAY', 'ARRAY'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OPEN1 = F
      OPEN2 = F
C                                       Get CATBLK for first file in
C                                       CATTMP.
      CALL CATIO ('READ', VOL1, CNO1, CATTMP, REST, BUFF1, IER)
      IF ((IER.GT.0) .AND. (IER.LT.5)) THEN
         IRET = 1
         WRITE (MSGTXT,1000) IER
         GO TO 990
         END IF
C                                       Open first (input) file
      MSGSUP = 32000
      IRET = 0
      NREC = 100
      CALL TABINI ('READ', TYPE, VOL1, CNO1, INVER, CATTMP, LUN1,
     *   NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
      MSGSUP = 0
C                                       If not there - quit
      IF (IER.EQ.2) GO TO 999
      IF (IER.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1010) IER
         GO TO 990
         END IF
      OPEN1 = T
C                                       Find source id column.
      CALL FNDCOL (1, SIDKEY(1), 10, T, BUFF1, SOUKOL, IER)
      IF (IER.NE.0) CALL FNDCOL (1, SIDKEY(2), 7, T, BUFF1, SOUKOL, IER)
      GOTSOU = IER.EQ.0
      IF (GOTSOU) SOUKOL = DATP(SOUKOL,1,1)
C                                       Offset time?
      DOTIME = ABS (TOFF).GT.1.0E-5
      DOTIMR = F
      DOTIMD = F
      NOTIME = 0
      IF (DOTIME) THEN
         CALL FNDCOL (1, SIDKEY(3), 8, T, BUFF1, TIMKOL, IER)
         IF (IER.NE.0) CALL FNDCOL (1, SIDKEY(4), 10, T, BUFF1, TIMKOL,
     *      IER)
         IF (IER.NE.0) THEN
            DOTIME = F
         ELSE
            NOTIME = DATP(TIMKOL,2,1) / 10
            IT2 = DATP(TIMKOL,2,1) - NOTIME * 10
            NOTIME = MAX (1, NOTIME)
            DOTIMD = DOTIME .AND. (IT2.EQ.1)
            DOTIMR = DOTIME .AND. (IT2.EQ.2)
            TIMKOL = DATP(TIMKOL,1,1)
            END IF
         END IF
C                                       Does output file exist
      CALL ISTAB (TYPE, VOL2, CNO2, OUTVER, LUN2, BUFF2, TABLE, EXIST,
     *   FITASC, IER)
      NEW = .NOT.EXIST
C                                       create output and fill
      IF (NEW) THEN
         CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
         IF (IER.GT.0) THEN
            MSGTXT = 'DBCAPP: FAILS TO CLOSE INPUT TABLE FOR COPY'
            GO TO 990
            END IF
         OPEN1 = F
         CALL TABCOP (TYPE, INVER, OUTVER, LUN1, LUN2, VOL1, VOL2, CNO1,
     *      CNO2, CATBLK, BUFF1, BUFF2, IER)
         IF (IER.NE.0) THEN
            MSGTXT = 'DBCAPP: FAILS TO MAKE NEW TABLE BY COPY'
            GO TO 990
            END IF
         NREC = 100
         CALL TABINI ('READ', TYPE, VOL1, CNO1, INVER, CATTMP, LUN1,
     *      NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
         IF (IER.NE.0) THEN
            MSGTXT = 'DBCAPP: FAILS TO REOPEN INPUT TBLE AFTER COPY'
            GO TO 990
            END IF
         OPEN1 = T
         END IF
C                                       Open second (output) file
      NREC = 100
      CALL TABINI ('WRIT', TYPE, VOL2, CNO2, OUTVER, CATBLK, LUN2,
     *   NKEY, NREC, NCOL, DATP(1,1,2), BUFF2, IER)
      IF (IER.GT.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IER
         GO TO 990
         END IF
      OPEN2 = T
C                                       Check table data.
      DO 50 I = 1,NCOL
         IF (DATP(I,2,2).NE.DATP(I,2,1)) THEN
            IRET = 5
            MSGTXT = 'DBCAPP: INPUT TABLES DO NOT MATCH'
            GO TO 990
            END IF
 50      CONTINUE
C                                       Tables don't match
C                                       Get number of records in the
C                                       files.
      NRECIN = BUFF1(5)
      NRECOU = BUFF2(5)
      OUTREC = NRECOU + 1
      IF (REWRIT .OR. NEW) OUTREC = 1
      IF (REWRIT) THEN
         WRITE (MSGTXT,1050) TYPE, OUTVER
      ELSE
         WRITE (MSGTXT,1051) TYPE, OUTVER
         END IF
      CALL MSGWRT (2)
C                                       Mark unsorted
      BUFF2(43) = 0
      BUFF2(44) = 0
C                                       Copy
      NEWSID = (NUMSOU.GT.0) .AND. GOTSOU
      IRET = 6
      DO 200 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1070) IER, 'READ', TYPE
            GO TO 990
            END IF
C                                       Source translate
         IF (NEWSID) THEN
            SUID = RECORD(SOUKOL)
            IF (SUID.GT.0) RECORD(SOUKOL) = SOUTRA(SUID)
            END IF
C                                       Modify time?
         IF (DOTIMR) THEN
            DO 150 IT = 1,NOTIME
               RECR(TIMKOL+IT-1) = RECR(TIMKOL+IT-1) + TOFF
 150           CONTINUE
            END IF
         IF (DOTIMD) THEN
            DO 160 IT = 1,NOTIME
               RECD(TIMKOL+IT-1) = RECD(TIMKOL+IT-1) + TOFF
 160           CONTINUE
            END IF
         CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
            GO TO 990
            END IF
         OUTREC = OUTREC + 1
 200     CONTINUE
C                                       Close tables.
      CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
         GO TO 990
         END IF
      OPEN1 = F
      CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
         GO TO 990
         END IF
      OPEN2 = F
      IRET = 0
      IF (NEW) CALL CATIO ('UPDT', VOL2, CNO2, CATBLK, REST, BUFF1,
     *   IER)
      IF (IER.NE.0) IRET = 5
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      IF (OPEN1) CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (OPEN2) CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DBCAPP: ERROR',I5,' READING CATBLK')
 1010 FORMAT ('DBCAPP: ERROR',I5,' OPENING OUTPUT TABLE')
 1020 FORMAT ('DBCAPP: ERROR',I5,' OPENING INPUT TABLE')
 1050 FORMAT ('Updating  data in output ',A,' table version',I3)
 1051 FORMAT ('Appending data to output ',A,' table version',I3)
 1070 FORMAT ('DBCAPP: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END

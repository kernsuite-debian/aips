LOCAL INCLUDE 'DBCON.INC'
C                                       Local include for DBCON
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MAXSOU, MXANT
C                                       MAXSOU = size of source table
      PARAMETER (MAXSOU=2000)
C                                       MXANT = max no antennas
      PARAMETER (MXANT=MAXANT)
      INTEGER   SEQ1, SEQ2, SEQ3, DISK1, DISK2, DISK3, BUFSZ1, BUFSZ2,
     *   CAT1(256), CAT2(256), MAXA(2), FIXPOL, POLANT(4),
     *   I1LOCU, I1LOCV, I1LOCW, I1LOCT, I1LOCB, I1LOCS, I1LOCQ,
     *   I1LOCI, I1LOCD, I1LOCA, I1LOC1, I1LOC2,
     *   J1LOCC, J1LOCS, J1LOCF, J1LOCD, J1LOCR, J1LOCI, INCS1, INCF1,
     *   INCIF1, ICOR01, NPARM1, LREC1, NCOR1, TYUVD1,
     *   I2LOCU, I2LOCV, I2LOCW, I2LOCT, I2LOCB, I2LOCS, I2LOCQ,
     *   I2LOCI, I2LOCD, I2LOCA, I2LOC1, I2LOC2,
     *   J2LOCC, J2LOCS, J2LOCF, J2LOCR, J2LOCD, J2LOCI, INCS2, INCF2,
     *   INCIF2, ICOR02, NPARM2, LREC2, NCOR2, TYUVD2,
     *   IORD(2,100), JORD(2,MAXCIF), NUMSOU, SOUTRA(MAXSOU),
     *   FQ2TRA(MAXFQ), DOTRAN, IBUFF1(UVBFSL,2), IBUFF2(UVBFSL)
      INTEGER   NVIS1, NVIS2, NNIF
      LOGICAL   ISCMP, DOUVM1, DOUVM2, ISCMP1, ISCMP2, MULTI
      REAL      XS1, XD1, XS2, XD2, XS3, XD3, REWT(2), DAYOFF(2),
     *   DOPOS, DOFRQ, DUMC(6), DOARR, XFQTOL, XCENT,
     *   BUFF1(UVBFSL,2), BUFF2(UVBFSL), TBUFF(UVBFSS), XBUFF(UVBFSS),
     *   CATR1(256), CATR2(256), DEQUI1, DEQUI2
      HOLLERITH XNAME1(3), XCLAS1(2), XNAME2(3), XCLAS2(2), XNAME3(3),
     *   XCLAS3(2), CATH1(256), CATH2(256)
      CHARACTER NAME1*12, CLASS1*6, NAME2*12, CLASS2*6,
     *   NAME3*12, CLASS3*6, NEWRD*8
      DOUBLE PRECISION FREQ1, FREQ2, RA1, RA2, DEC1, DEC2, UVMUL1,
     *   UVMUL2, CATD1(128), CATD2(128), DELTC(2), DELTF(2)
      EQUIVALENCE (BUFF1, IBUFF1), (BUFF2, IBUFF2)
      COMMON /INPARM/ XNAME1, XCLAS1, XS1, XD1, XNAME2, XCLAS2, XS2,
     *   XD2, REWT, XNAME3, XCLAS3, XS3, XD3, DOPOS, DOFRQ, DUMC, DOARR,
     *   XFQTOL, XCENT
      COMMON /DBPARM/ SEQ1, SEQ2, SEQ3, DISK1, DISK2, DISK3, BUFSZ1,
     *   BUFSZ2, ISCMP, DAYOFF, ISCMP1, ISCMP2, MULTI
      EQUIVALENCE (CAT1, CATR1, CATH1, CATD1),
     *   (CAT2, CATR2, CATH2, CATD2)
      COMMON /BUFRS/ BUFF1, BUFF2, TBUFF, XBUFF
      COMMON /CHRCOM/ NAME1, CLASS1, NAME2, CLASS2, NAME3, CLASS3, NEWRD
C                                       This common MUST match the one
C                                       in DUVH.INC
      COMMON /CATHDR/ CAT1, CAT2, UVMUL1, UVMUL2, DELTC, DELTF,
     *   DOUVM1, DOUVM2,
     *   FREQ1, RA1, DEC1, NVIS1, I1LOCU, I1LOCV, I1LOCW, I1LOCT,
     *   I1LOCB, I1LOCS, I1LOCQ, I1LOCI, I1LOCD, I1LOCA, I1LOC1, I1LOC2,
     *   J1LOCC, J1LOCS, J1LOCF, J1LOCR, J1LOCD, J1LOCI, INCS1, INCF1,
     *   INCIF1, ICOR01, NPARM1, LREC1, NCOR1, TYUVD1, DEQUI1,
     *   FREQ2, RA2, DEC2, NVIS2, I2LOCU, I2LOCV, I2LOCW, I2LOCT,
     *   I2LOCB, I2LOCS, I2LOCQ, I2LOCI, I2LOCD, I2LOCA, I2LOC1, I2LOC2,
     *   J2LOCC, J2LOCS, J2LOCF, J2LOCR, J2LOCD, J2LOCI, INCS2, INCF2,
     *   INCIF2, ICOR02, NPARM2, LREC2, NCOR2, TYUVD2, DEQUI2,
     *   IORD, JORD, MAXA, FIXPOL, POLANT, NUMSOU, SOUTRA, NNIF, FQ2TRA,
     *   DOTRAN
C                                                          End DBCON
LOCAL END
      PROGRAM DBCON
C-----------------------------------------------------------------------
C! Concatenates uv data files
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2004, 2006-2012, 2014-2017
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
C      INNAME      NAME1          First uv file name
C      INCLASS     CLASS1         First uv file class
C      INSEQ       SEQ1           First uv file sequence no.
C      INDISK      DISK1          First uv file disk number.
C      IN2NAME     NAME2          Second uv file name
C      IN2CLASS    CLASS2         Second uv file class
C      IN2SEQ      SEQ2           Second uv file sequence no.
C      IN2DISK     DISK2          Second uv file disk number.
C      REWEIGHT    REWT           Weight scaling factors
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
C      DOARRAY     DOARR          If < 0 write data as subarrays
C                                 If > 0 write both inputs as one
C                                 subarray.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, IERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DBCON.INC'
      DATA PRGM /'DBCON '/
C-----------------------------------------------------------------------
C                                       Get inputs.
      IRET = 8
      CALL DBCIN (PRGM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check data compatibility and
C                                       determine output format.
      CALL FRMAT (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Merge source tables.
      CALL DBSOUR (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Merge AN tables without
C                                       renumbering antennas.
      CALL DBANT (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Check FQ table consistency.
      CALL DBIF (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Sequential copy
      IF (ISORT.EQ.'**') THEN
         CALL DBCOPY (IERR)
C                                       Merge copy
      ELSE
         CALL DBMNGL (IERR)
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Copy history, antenna files.
      CALL HISANT
      IRET = 0
C                                        Close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DBCIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   DBCIN gets inputs for DBCON and marks input files 'READ'
C   Inputs: PRGM(2)   C*6   Task name
C   Output: IRET      I     Return error code, 0 = OK,
C                                              1 = Error, abort.
C                                              2 = only 1 multisource
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, BLANK*6, STAT*4, UTYPE*2, CHDATE*8
      INTEGER   CNO, IRET, IROUND, NPARM, IERR, LUN
      LOGICAL   T, F, TABLE, FITASC, IS1MS, IS2MS
      DOUBLE PRECISION JD1, JD2, JDR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'DBCON.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /27/
      DATA BLANK /' '/
C-----------------------------------------------------------------------
C                                        Initialize
      IRET = 0
      CALL ZDCHIN (T)
      CALL VHDRIN
      NSCR = 0
      NCFILE = 0
      BUFSZ1 = UVBFSL * 2
      BUFSZ2 = UVBFSL * 2
C                                        Get input.
      NPARM = 34
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         IRET = 8
         RQUICK = F
         GO TO 990
         END IF
C                                        Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFF1, IERR)
C                                        Crunch input.
      SEQ1 = IROUND (XS1)
      SEQ2 = IROUND (XS2)
      SEQ3 = IROUND (XS3)
      DISK1 = IROUND (XD1)
      DISK2 = IROUND (XD2)
      DISK3 = IROUND (XD3)
      CALL FILL (10, 0, IBAD)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAME1, NAME1)
      CALL H2CHR (6, 1, XCLAS1, CLASS1)
      CALL H2CHR (12, 1, XNAME2, NAME2)
      CALL H2CHR (6, 1, XCLAS2, CLASS2)
      CALL H2CHR (12, 1, XNAME3, NAME3)
      CALL H2CHR (6, 1, XCLAS3, CLASS3)
C                                        Find input files and get CAT
      IRET = 8
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK1, CNO, NAME1, CLASS1, SEQ1, UTYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, NAME1, CLASS2, SEQ1, 'UV', DISK1,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK1, CNO, CAT1, 'READ', BUFF1, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                        Update /CFILE/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       See if multisource
      CALL MULSDB (CAT1, IS1MS)
      IF (IS1MS) THEN
         CALL ISTAB ('SU', DISK1, CNO, 1, LUN, BUFF1, TABLE, IS1MS,
     *      FITASC, IERR)
         IS1MS = IS1MS .AND. (IERR.EQ.0)
         END IF
C                                        Get second input file.
      CNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISK2, CNO, NAME2, CLASS2, SEQ2, UTYPE,
     *   NLUSER, STAT, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, NAME2, CLASS2, SEQ2, 'UV', DISK2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISK2, CNO, CAT2, 'READ', BUFF1, IRET)
      IF (IRET.GT.1) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                        Update /CFILE/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 0
C                                       See if multisource
      CALL MULSDB (CAT2, IS2MS)
      IF (IS2MS) THEN
         CALL ISTAB ('SU', DISK2, CNO, 1, LUN, BUFF1, TABLE, IS2MS,
     *      FITASC, IERR)
         IS2MS = IS2MS .AND. (IERR.EQ.0)
         END IF
C                                        Set def. outname to inname 1
      CALL MAKOUT (NAME1, CLASS1, SEQ1, BLANK, NAME3, CLASS3, SEQ3)
      IRET = 0
C                                       Both multi-source
      MULTI = .FALSE.
      IF ((IS1MS) .AND. (IS2MS)) THEN
C                                       All data in the same subarray
         DOARR = 1.0
C                                       Do not shift positions
         DOPOS = 0.0
         MULTI = .TRUE.
C                                       Can't combine
      ELSE IF ((IS1MS) .OR. (IS2MS)) THEN
         IRET = 2
         MSGTXT = 'CANNOT COMBINE SINGLE AND MULTISOURCE FILES'
         GO TO 990
         END IF
C                                       Default reweighting factors = 1
      IF (REWT(1).LE.0.0) REWT(1) = 1.0
      IF (REWT(2).LE.0.0) REWT(2) = 1.0
C                                       change reference days?
      CALL H2CHR (8, 1, CATH1(KHDOB), NEWRD)
      DAYOFF(1) = 0.0
      DAYOFF(2) = 0.0
      IF (DOARR.GT.0) THEN
         CALL H2CHR (8, 1, CATH1(KHDOB), CHDATE)
         CALL JULDAY (CHDATE, JD1)
         CALL H2CHR (8, 1, CATH2(KHDOB), CHDATE)
         CALL JULDAY (CHDATE, JD2)
         JDR = MIN (JD1, JD2)
         DAYOFF(1) = JD1 - JDR
         DAYOFF(2) = JD2 - JDR
         CALL GREG (JDR, NEWRD)
         IF (JD1.NE.JD2) THEN
            WRITE (MSGTXT,1900) NEWRD
            CALL MSGWRT (5)
            END IF
         END IF
      GO TO 999
C                                        Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I2,' USER=',I4)
 1030 FORMAT ('ERROR',I3,' READING CATALOG HEADER FOR INPUT FILE')
 1900 FORMAT ('New reference day is ',A)
      END
      SUBROUTINE FRMAT (IRET)
C-----------------------------------------------------------------------
C   FRMAT determines the compatibility of files, determines the output
C   format and the translation tables for the three formats and creates
C   the output file.
C   Output: IRET    I     Return error code, 0 => OK, else abort.
C-----------------------------------------------------------------------
      CHARACTER BLANK*8, CHSIN*4, XRA*8, XDEC*8, PROJ1*4, PROJ2*4,
     *   CHFREQ*8, STOKES*8, XTYP*8, CHTM12*12, CHTM8*8, KEYWRD*8,
     *   KEYSKP(2)*8
      INTEGER   ARRAY1(1), ARRAY2(1), ARRAY3(1), JPOS(2,10), IOFF(10,2),
     *   INCO(10), INCI(10), ISGN(10,2), I1SGN, I2SGN, LIMO, IPASS, II,
     *   III, KRAN, IRET, IERR, NWORDS, LIM, I, NRAN, INDEX, LIM2, J,
     *   JNDX, JJ, NUP1, NUP2, NAXIS, JP, I1HI, I1LO, I2HI, I2LO, L,
     *   NAXES, JTRIM, LOCS, VALUE, KEYTYP, CORCO1, CORCO2, NUMKEY
      LOGICAL   F, EQUAL, GOTF1, GOTF2, GOTS1, GOTS2
      REAL      R, DOIPOS, REFPIX
      DOUBLE PRECISION FR1, FR2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBCON.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ARRAY1, FREQ1),  (ARRAY2, FREQ2),  (ARRAY3, FREQ)
      DATA  BLANK, CHSIN /' ', '-SIN'/
      DATA XRA, XDEC /'RA ','DEC '/
      DATA F /.FALSE./
      DATA KEYSKP /'MAXBLINE','MAXABSU'/
C-----------------------------------------------------------------------
      IRET = 1
      DOIPOS = -1.
      ISCMP1 = CAT1(KINAX).EQ.1
      ISCMP2 = CAT2(KINAX).EQ.1
      ISCMP = ISCMP1 .OR. ISCMP2
C                                       get header keyword CORRCOEF
      KEYWRD = 'CORRCOEF'
      NUMKEY = 1
      CALL CATKEY ('REED', FVOL(1), FCNO(1), KEYWRD, NUMKEY, LOCS,
     *   VALUE, KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         CORCO1 = 0
      ELSE
         CORCO1 = VALUE
         END IF
      NUMKEY = 1
      CALL CATKEY ('REED', FVOL(2), FCNO(2), KEYWRD, NUMKEY, LOCS,
     *   VALUE, KEYTYP, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         CORCO2 = 0
      ELSE
         CORCO2 = VALUE
         END IF
      IF (CORCO1.NE.CORCO2) THEN
         IF ((CORCO1.NE.0) .AND. (CORCO2.NE.0)) THEN
            MSGTXT = 'CANNOT COMBINE CORRELATION COEFFICIENTS WITH'
     *         // ' VISIBILITIES'
            GO TO 990
         ELSE IF ((CORCO1.EQ.1) .OR. (CORCO2.EQ.1)) THEN
            MSGTXT = 'WARNING: COMBINING CORR. COEFFICIENTS WITH'
     *         // 'DATA OF UNKNOWN TYPE'
            CALL MSGWRT (7)
         ELSE
            MSGTXT = 'WARNING: COMBINING VISIBILITY DATA WITH DATA'
     *         // 'OF UNKNOWN TYPE'
            CALL MSGWRT (7)
            END IF
         END IF
C                                        Crunch header info
C                                        Header # 2.
      CALL COPY (256, CAT2, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Projection type
      INDEX = KHPTP + ILOCU * 2
      CALL H2CHR (4, 5, CATH2(INDEX), CHTM12)
      IF ((TYPUVD.LE.0) .AND. (BLANK(1:4).EQ.CHTM12(1:4))) THEN
C                                       Default projection=-SIN
         CALL CHR2H (4, CHSIN(1:4), 5, CATH2(INDEX))
C                                       V
         INDEX = KHPTP + ILOCV * 2
         CALL CHR2H (4, CHSIN(1:4), 5, CATH2(INDEX))
C                                       W
         INDEX = KHPTP + ILOCW * 2
         CALL CHR2H (4, CHSIN(1:4), 5, CATH2(INDEX))
         END IF
C                                       Save projection type
      CALL H2CHR (4, 5, CATH2(INDEX), PROJ2(1:4))
C                                       No projection on RA
      INDEX = KHCTP + JLOCR * 2
      IF (JLOCR.GE.0) CALL CHR2H (8, XRA, 1, CATH2(INDEX))
C                                       No projection on Dec
      INDEX = KHCTP + JLOCD * 2
      IF (JLOCD.GE.0) CALL CHR2H (8, XDEC, 1, CATH2(INDEX))
C                                       Convert ref. freq. to pixel
      IF (XCENT.LE.0.0) THEN
         REFPIX = 1.0
         IF ((CATR1(KRCRP+JLOCF).EQ.CATR2(KRCRP+JLOCF)))
     *      REFPIX = CATR1(KRCRP+JLOCF)
      ELSE
         REFPIX = CATBLK(KINAX+JLOCF) / 2 + 1
         END IF
      DOUVM2 = ABS (REFPIX-CATR2(KRCRP+JLOCF)).GT.0.01
      DOUVM2 = DOUVM2 .AND. (TYPUVD.LE.0)
      UVMUL2  = CATD2(KDCRV+JLOCF)
      DELTC(2) = 0.0D0
      DELTF(2) = 0.0D0
      IF (DOUVM2) THEN
         DELTC(2) = (REFPIX - CATR2(KRCRP+JLOCF))
         DELTF(2) = DELTC(2) * CATR2(KRCIC+JLOCF)
         CATD2(KDCRV+JLOCF) = CATD2(KDCRV+JLOCF) + DELTF(2)
         CATR2(KRCRP+JLOCF) = REFPIX
         END IF
      FREQ = CATD2(KDCRV+JLOCF)
      FR2 = FREQ
      UVMUL2 = FREQ / UVMUL2
C                                        Fill values in /CATHDR/
      NWORDS = 28 + 3 * NWDPDP
      CALL COPY (NWORDS, ARRAY3, ARRAY2)
C                                       determine number of IF
      IF (J2LOCI.EQ.-1) THEN
         NNIF = 1
      ELSE
         NNIF = CAT2(KINAX+J2LOCI)
         END IF
C                                        Header #1, leave in CATBLK
      CALL COPY (256, CAT1, CATBLK)
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Projection type
      INDEX = KHPTP + ILOCU * 2
      CALL H2CHR (4, 5, CATH1(INDEX), CHTM12)
      IF ((TYPUVD.LE.0) .AND. (BLANK(1:4).EQ.CHTM12(1:4))) THEN
C                                       Default projection=-SIN
         CALL CHR2H (4, CHSIN(1:4), 5, CATH1(INDEX))
         CALL CHR2H (4, CHSIN(1:4), 5, CATH(INDEX))
C                                       V
         INDEX = KHPTP + ILOCV * 2
         CALL CHR2H (4, CHSIN(1:4), 5, CATH1(INDEX))
         CALL CHR2H (4, CHSIN(1:4), 5, CATH(INDEX))
C                                       W
         INDEX = KHPTP + ILOCW * 2
         CALL CHR2H (4, CHSIN(1:4), 5, CATH1(INDEX))
         CALL CHR2H (4, CHSIN(1:4), 5, CATH(INDEX))
         END IF
C                                       Save projection type
      CALL H2CHR (4, 5, CATH1(INDEX), PROJ1(1:4))
C                                       No projection on RA
      IF (JLOCR.GE.0) THEN
         INDEX = KHCTP + JLOCR * 2
         CALL CHR2H (8, XRA, 1, CATH1(INDEX))
         CALL CHR2H (8, XRA, 1, CATH(INDEX))
         END IF
C                                       No projection on Dec
      IF (JLOCD.GE.0) THEN
         INDEX = KHCTP + JLOCD * 2
         CALL CHR2H (8, XDEC, 1, CATH1(INDEX))
         CALL CHR2H (8, XDEC, 1, CATH(INDEX))
         END IF
C                                       Convert ref. freq. to pixel 1
C                                       Convert ref. freq. to pixel
      IF (XCENT.LE.0.0) THEN
         REFPIX = 1.0
         IF ((CATR1(KRCRP+JLOCF).EQ.CATR2(KRCRP+JLOCF)))
     *      REFPIX = CATR1(KRCRP+JLOCF)
      ELSE
         REFPIX = CATBLK(KINAX+JLOCF) / 2 + 1
         END IF
      DOUVM1 = ABS (REFPIX-CATR1(KRCRP+JLOCF)).GT.0.01
      DOUVM1 = DOUVM1 .AND. (TYPUVD.LE.0)
      UVMUL1 = CATD1(KDCRV+JLOCF)
      DELTC(1) = 0.0D0
      DELTF(1) = 0.0D0
      IF (DOUVM1) THEN
         DELTC(1) = (REFPIX - CATR1(KRCRP+JLOCF))
         DELTF(1) = DELTC(1) * CATR1(KRCIC+JLOCF)
         CATD1(KDCRV+JLOCF) = CATD1(KDCRV+JLOCF) + DELTF(1)
         CATR1(KRCRP+JLOCF) = REFPIX
         END IF
      CATD(KDCRV+JLOCF) = CATD1(KDCRV+JLOCF)
      CATR(KRCRP+JLOCF) = CATR1(KRCRP+JLOCF)
      FREQ = CATD1(KDCRV+JLOCF)
      FR1 = FREQ
      UVMUL1 = FREQ / UVMUL1
C                                       DO NOT DO THIS VERY WRONG
C     DOUVM2 = ABS (FREQ-UVMUL2).GT.(0.01 * ABS (CATR1(KRCIC+JLOCF)))
C     UVMUL2 = FREQ / UVMUL2
      WRITE (MSGTXT,4000) 1, UVMUL1
      IF (DOUVM1) CALL MSGWRT (6)
      WRITE (MSGTXT,4000) 2, UVMUL2
      IF (DOUVM2) CALL MSGWRT (6)
      IF (ABS(FR1-FR2).GT.0.1*ABS(CATR1(KRCIC+JLOCF))) THEN
         MSGTXT = '**************************************************'
         CALL MSGWRT (8)
         MSGTXT = 'YOU ARE COMBINING DATA SETS OF DIFFERENT FREQUENCY'
         CALL MSGWRT (8)
         MSGTXT = 'SINCE FREQUENCY INFO IS LOST, DO NOT USE TASKS ON'
         CALL MSGWRT (8)
         MSGTXT = 'THE OUTPUT THAT CHANGE U-V-W, ESPECIALLY UVFIX'
         CALL MSGWRT (8)
         MSGTXT = '**************************************************'
         CALL MSGWRT (8)
         END IF
C                                        Fill values in /CATHDR/
      CALL COPY (NWORDS, ARRAY3, ARRAY1)
C                                       Check compatibility
C                                       Different types
      IF (TYUVD1.NE.TYUVD2) THEN
         MSGTXT = 'UV DATA TYPES ARE INCOMPATIBLE'
         GO TO 990
         END IF
      IF (TYUVD1.GT.0) DOPOS = 0.
C                                       Not same projection.
      IF (PROJ1(1:4).NE.PROJ2(1:4)) THEN
         MSGTXT = 'U,V AND W PROJECTIONS DO NOT MATCH'
         GO TO 990
         END IF
      IF ((.NOT.MULTI) .AND. (TYPUVD.LE.0)) THEN
C                                       Check Epoch
         IF (CATR1(KREPO).NE.CATR2(KREPO)) THEN
            WRITE (MSGTXT,1000) CATR1(KREPO), CATR2(KREPO)
            IF (DOPOS.GT.0.0) GO TO 990
            CALL MSGWRT (7)
            END IF
C                                        Check RA
         IF (ABS(RA1-RA2).GT.3.0E-10) THEN
            IF (DOPOS.GT.0.0) THEN
               MSGTXT= 'RA does not match, will shift second'
               CALL MSGWRT (3)
               DOIPOS = 1.
            ELSE
               MSGTXT= 'RA DOES NOT MATCH, BUT NOT DOING SHIFTS!'
               CALL MSGWRT (7)
               END IF
            END IF
C                                        Check Dec.
         IF (ABS(DEC1-DEC2).GT.3.0E-10) THEN
            IF (DOPOS.GT.0.0) THEN
               MSGTXT = 'DEC does not match, will shift second'
               CALL MSGWRT (3)
               DOIPOS = 1.
            ELSE
               MSGTXT= 'DEC DOES NOT MATCH, BUT NOT DOING SHIFTS!'
               CALL MSGWRT (7)
               END IF
            END IF
         IF ((DOIPOS.LT.0.0) .AND. (DOPOS.GT.0.0)) THEN
            MSGTXT = 'RA and DEC match to within 1 microarcsecond'
     *         // ' each'
            CALL MSGWRT (3)
            MSGTXT = '--I''m not going to shift positions'
            CALL MSGWRT (3)
            END IF
         END IF
C                                        Check random parameters.
C                                        Fill random parm. pointers
      LIM = MAX (NPARM1, NPARM2)
      DO 40 I = 1,LIM
         IORD(1,I) = I - 1
         IORD(2,I) = I - 1
 40      CONTINUE
      NRAN = NPARM1
      KRAN = NPARM1
C                                       Munge random parameters #2.
C                                       Mixed new and old format
      IF ((I1LOCB.GE.0) .AND. (I2LOCB.LT.0)) THEN
         JNDX = KHPTP + I2LOCA*2
         CALL CHR2H (8, 'BASELINE', 1, CATH2(JNDX))
         JNDX = KHPTP + I2LOC1*2
         CALL CHR2H (8, 'BASELINE', 1, CATH2(JNDX))
         JNDX = KHPTP + I2LOC2*2
         CALL CHR2H (8, 'BASELINE', 1, CATH2(JNDX))
         DOTRAN = 1
      ELSE IF ((I1LOCB.LT.0) .AND. (I2LOCB.GT.0)) THEN
         JNDX = KHPTP + I2LOCB*2
         CALL CHR2H (8, 'SUBARRAY', 1, CATH2(JNDX))
         DOTRAN = -1
      ELSE
         DOTRAN = 0
         END IF
C                                        Munge random parameters #2.
      DO 60 I = 1,NPARM2
         INDEX = KHPTP + (I-1)*2
         LIM2 = MIN (KIPTPN, NRAN)
         IF (I.LE.KIPTPN) THEN
            DO 50 J = 1,LIM2
               JNDX = KHPTP + (J-1)*2
               JJ = J
               CALL CHCOMP (8, 1, CATH(JNDX), 1, CATH2(INDEX), EQUAL)
C                                        "OLD" parameter
               IF (EQUAL) THEN
                  IORD(2,I) = JJ - 1
                  GO TO 60
                  END IF
 50            CONTINUE
            END IF
C                                       "NEW" random parameters.
         JNDX = KHPTP + NRAN*2
         KRAN = KRAN + 1
         IF (NRAN.LE.KIPTPN) THEN
            NRAN = NRAN + 1
            CALL CHCOPY (8, 1, CATH2(INDEX), 1, CATH(JNDX))
            END IF
         IORD(2,I) = KRAN - 1
 60      CONTINUE
C                                        Check that number of unlabeled
C                                        parms are the same.
      IF ((NRAN.LE.KIPTPN) .OR. (NPARM1.EQ.NPARM2)) GO TO 100
C                                       Allow 1 unmatched, unlabeled
C                                       random parameter.
      IF (NRAN.NE.(KIPTPN+1)) THEN
         NUP1 = MAX (0, NPARM1-KIPTPN)
         NUP2 = MAX (0, NPARM2-KIPTPN)
         WRITE (MSGTXT,1070) NUP1, NUP2
         GO TO 990
         END IF
C                                        Set # random parms
C                                        in output.
 100  NRPARM = NRAN
      IF (DOIPOS.LT.0.0) DOPOS = -1.0
C                                        Prepare output CATBLK for
C                                        random parameters.
      CATBLK(KIPCN) = NRPARM
C                                        Uniform parameters.
C                                        Fill #1 output posn array.
      DO 105 I = 1,KICTPN
         JPOS(1,I) = I
 105     CONTINUE
C                                        Munge uniform parm #2.
      NAXIS = CATBLK(KIDIM)
      LIM = CAT2(KIDIM)
      DO 130 I = 1,LIM
         INDEX = KHCTP + (I-1)*2
         LIM2 = MIN (KICTPN, NAXIS)
         DO 110 J = 1,LIM2
            JNDX = KHCTP + (J-1)*2
            JJ = J
            CALL CHCOMP (8, 1, CATH(JNDX), 1, CATH2(INDEX), EQUAL)
            IF (EQUAL) GO TO 120
 110        CONTINUE
C                                        "new" in #2
         JNDX = KHCTP + NAXIS*2
         NAXIS = NAXIS + 1
         IF (NAXIS.LE.KICTPN) CALL CHCOPY (8, 1, CATH2(INDEX), 1,
     *      CATH(JNDX))
         JPOS(2,I) = NAXIS
         JP = JPOS(2,I) - 1
         CATBLK(KINAX+JP) = CAT2(KINAX+I-1)
         CATD(KDCRV+JP) = CATD2(KDCRV+I-1)
         CATR(KRCIC+JP) = CATR2(KRCIC+I-1)
         CATR(KRCRP+JP) = CATR2(KRCRP+I-1)
         CATR(KRCRT+JP) = CATR2(KRCRT+I-1)
         IF (CAT2(KINAX+I-1).LE.1) GO TO 130
C                                      Illegal value, abort.
            INDEX = KHCTP + (I-1)*2
            CALL H2CHR (8, 1, CATH(INDEX), XTYP)
            WRITE (MSGTXT,1110) XTYP, CAT2(KINAX+I-1)
            GO TO 990
C                                        "OLD"
 120     CONTINUE
            JPOS(2,I) = JJ
 130     CONTINUE
C                                        Check limit of KICTPN axes.
      IF (NAXIS.GT.KICTPN) THEN
         WRITE (MSGTXT,1130) NAXIS, KICTPN
         GO TO 990
         END IF
      CATBLK(KIDIM) = NAXIS
      IF (ISCMP) CATBLK(KINAX) = 1
C                                        Check axis attributes:
C                                        Must be the same except for
C                                        Stokes and Frequency
      CHFREQ = 'FREQ'
      STOKES = 'STOKES'
C                                        Non-overlapping axis types
C                                        must have dimension 1
C                                        Check.
      LIM = CAT1(KIDIM)
      DO 170 I = 1,LIM
         LIM2 = CAT2(KIDIM)
         DO 150 J = 1,LIM2
            JP = J
            IF (JPOS(2,J).EQ.I) GO TO 160
 150        CONTINUE
C                                        Type in #1 not in 2
         IF (CATBLK(KINAX+I-1).LE.1) GO TO 170
            INDEX = KHCTP + (I-1)*2
            CALL H2CHR (8, 1, CATH(INDEX), XTYP)
            WRITE (MSGTXT,1110) XTYP, CATBLK(KINAX+I-1)
            GO TO 990
 160        CONTINUE
C                                        Found match.
C                                        Check if stokes
C                                        or frequency.
         INDEX = KHCTP + (I-1)*2
         CALL H2CHR (8, 1, CATH(INDEX), CHTM8)
         L = JTRIM (CHTM8)
         IF (STOKES.EQ.CHTM8) GO TO 170
         IF (CHFREQ.EQ.CHTM8) GO TO 170
         IF ('COMPLEX '.EQ.CHTM8) GO TO 170
C                                        Check values: dimension.
            IF (CATBLK(KINAX+I-1).NE.CAT2(KINAX+JP-1)) GO TO 165
C                                        Coor. rotation.
            IF (ABS(CATR(KRCRT+I-1)-CATR2(KRCRT+JP-1)).GT.0.01)
     *         GO TO 165
C                                        (except RA or DEC)
            IF ((CHTM8.NE.'RA') .AND. (CHTM8.NE.'DEC')) THEN
C                                        Coordinate ref. value.
               IF (ABS ((CATD(KDCRV+I-1)-CATD2(KDCRV+JP-1)))
     *           .GT.(1.D-8 * ABS (CATD(KDCRV+I-1)))) GO TO 165
C                                        Coor. increment.
               IF (ABS (CATR(KRCIC+I-1)-CATR2(KRCIC+JP-1))
     *          .GT.(1.0E-4 * ABS (CATR (KRCIC+I-1)))) GO TO 165
C                                        Coor. ref. pixel
               IF (ABS (CATR(KRCRP+I-1)-CATR2(KRCRP+JP-1))
     *          .GT.(1.0E-4 * ABS (CATR (KRCRP+I-1)))) GO TO 165
C                                        Coor. rotation.
               IF (ABS (CATR(KRCRT+I-1)-CATR2(KRCRT+JP-1))
     *          .GT.(1.0E-4 * ABS (CATR (KRCRT+I-1)))) GO TO 165
               END IF
            GO TO 170
C                                        Not equal or special-abort
 165              CALL H2CHR (8, 1, CATH(INDEX), XTYP)
                  WRITE (MSGTXT,1165) XTYP
                  GO TO 990
 170     CONTINUE
C                                        Update output CATBLK pointers
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Check stokes and fix output
C                                        CATBLK. Stokes must be in both
C                                        or neither.
      GOTF1 = F
      GOTS1 = F
      LIM = CAT1(KIDIM)
      DO 210 I = 1,LIM
         INDEX = KHCTP + (I-1)*2
         CALL H2CHR (8, 1, CATH1(INDEX), CHTM8)
         L = JTRIM (CHTM8)
         GOTS1 = GOTS1 .OR. (STOKES.EQ.CHTM8)
         GOTF1 = GOTF1 .OR. (CHFREQ.EQ.CHTM8)
 210     CONTINUE
      GOTF2 = F
      GOTS2 = F
      LIM = CAT2(KIDIM)
      DO 220 I = 1,LIM
         INDEX = KHCTP + (I-1)*2
         CALL H2CHR (8, 1, CATH2(INDEX), CHTM8)
         L = JTRIM (CHTM8)
         GOTS2 = GOTS2 .OR. (STOKES.EQ.CHTM8)
         GOTF2 = GOTF2 .OR. (CHFREQ.EQ.CHTM8)
 220     CONTINUE
      CALL FILL (KICTPN, 1, ISGN(1,1))
      CALL FILL (KICTPN, 1, ISGN(1,2))
      CALL FILL (KICTPN, 0, IOFF(1,1))
      CALL FILL (KICTPN, 0, IOFF(1,2))
C                                        Only one data base has stokes
      IF ((GOTS1) .AND. (GOTS2)) GO TO 230
      IF ((.NOT.GOTS1) .AND. (.NOT.GOTS2)) GO TO 230
         WRITE (MSGTXT,1220)
         GO TO 990
C                                        Stokes given for both.
 230  IF (.NOT.GOTS1) GO TO 265
C                                       Check for standard type
         R = CATR1(KRCIC+J1LOCS)
         IF ((R.EQ.1.0) .OR. (R.EQ.-1.0) .OR. ((R.EQ.0.0) .AND.
     *      (CAT1(KINAX+J1LOCS).EQ.1))) GO TO 235
            GO TO 236
 235     R = CATR2(KRCIC+J2LOCS)
         IF ((R.EQ.1.0) .OR. (R.EQ.-1.0) .OR. ((R.EQ.0.0) .AND.
     *      (CAT1(KINAX+J1LOCS).EQ.1))) GO TO 237
 236        WRITE (MSGTXT,1236)
            GO TO 990
C                                       pick up values
 237     I1SGN = SIGN (1, ICOR01)
         I1HI = (ICOR01 + (CAT1(KINAX+J1LOCS)-1)*CATR1(KRCIC+J1LOCS)) *
     *      I1SGN
         I1LO = MIN (I1HI, ICOR01*I1SGN)
         I1HI = MAX (I1HI, ICOR01*I1SGN)
         I2SGN = SIGN (1, ICOR02)
         I2HI = (ICOR02 + (CAT2(KINAX+J2LOCS)-1)*CATR2(KRCIC+J2LOCS)) *
     *      I2SGN
         I2LO = MIN (I2HI, ICOR02*I2SGN)
         I2HI = MAX (I2HI, ICOR02*I2SGN)
         IF (I1SGN.NE.I2SGN) GO TO 250
C                                        Check that ranges overlap.
            IF ((I1LO.GE.I2LO) .AND. (I1LO.LE.I2HI)) GO TO 240
            IF ((I2LO.GE.I1LO) .AND. (I2LO.LE.I1HI)) GO TO 240
               WRITE (MSGTXT,1237)
               GO TO 990
 240        IOFF(J1LOCS+1,1) = ICOR01 * I1SGN - MIN (I1LO, I2LO)
            IOFF(J2LOCS+1,2) = ICOR02 * I2SGN - MIN (I1LO, I2LO)
            ISGN(J1LOCS+1,1) = I1SGN * CATR1(KRCIC+J1LOCS)
            ISGN(J2LOCS+1,2) = I2SGN * CATR2(KRCIC+J2LOCS)
            FIXPOL = 0
C                                       Fill output CATBLK with stokes
C                                       info.
            CATBLK(KINAX+JLOCS) = MAX (I1HI, I2HI) - MIN (I1LO, I2LO)
     *         + 1
            CATR(KRCIC+JLOCS) = I1SGN
            CATD(KDCRV+JLOCS) = I1SGN * MIN (I1LO, I2LO)
            ICOR0 = CATD(KDCRV+JLOCS) + 0.1
            CATR(KRCRP+JLOCS) = 1.0
            CATR(KRCRT+JLOCS) = 0.0
            GO TO 265
C                                       Opposite polarization types
C                                       Must convert to I, Q, U, V
 250     CONTINUE
            CATBLK(KINAX+JLOCS) = 4
            CATR(KRCIC+JLOCS) = 1.0
            ICOR0 = 1
            CATD(KDCRV+JLOCS) = 1.0D0
            CATR(KRCRP+JLOCS) = 1.0
            CATR(KRCRT+JLOCS) = 0.0
            IF (I1SGN.LT.0) GO TO 255
               IOFF(J1LOCS+1,1) = ICOR01 - 1
               ISGN(J1LOCS+1,1) = CATR1(KRCIC+J1LOCS)
               FIXPOL = 2
               DO 251 I = 1,4
                  POLANT(I) = 0
                  R = 1.0 - (I+ICOR02) / CATR2(KRCIC+J2LOCS)
                  L = R + 0.01
                  IF ((L.GE.1) .AND. (L.LE.CAT2(KINAX+J2LOCS)) .AND.
     *               (ABS(R-L).LT.0.1)) POLANT(I) = L
 251              CONTINUE
               GO TO 265
 255        CONTINUE
               IOFF(J2LOCS+1,1) = ICOR02 - 1
               ISGN(J2LOCS+1,1) = CATR2(KRCIC+J2LOCS)
               FIXPOL = 1
               DO 256 I = 1,4
                  POLANT(I) = 0
                  R = 1.0 - (I+ICOR01) / CATR1(KRCIC+J1LOCS)
                  L = R + 0.01
                  IF ((L.GE.1) .AND. (L.LE.CAT1(KINAX+J1LOCS)) .AND.
     *               (ABS(R-L).LT.0.1)) POLANT(I) = L
 256              CONTINUE
C                                       Only one data base has freq.
 265  IF ((GOTF1) .AND. (GOTF2)) GO TO 270
      IF ((.NOT.GOTF1) .AND. (.NOT.GOTF2)) GO TO 270
         WRITE (MSGTXT,1265)
         GO TO 990
C                                       Freq. given for both.
 270  IF (.NOT.GOTF1) GO TO 280
C                                       If more than 1 channel, they
C                                       must all agree.
         IF ((CAT1(KINAX+J1LOCF).EQ.1) .AND. (CAT2(KINAX+J2LOCF).EQ.1))
     *      GO TO 280
C                                       Require freq to agree to 0.001
            EQUAL = (ABS (CATD1(KDCRV+J1LOCF) - CATD2(KDCRV+J2LOCF)))
     *              .LE.(0.0004D0 * CATD1(KDCRV+J1LOCF))
            IF  ((CAT1(KINAX+J1LOCF).EQ.CAT2(KINAX+J2LOCF)) .AND.
     *         (EQUAL .OR. (DOFRQ.LE.0.0)) .AND.
     *         ((ABS (CATR1(KRCIC+J1LOCF) - CATR2(KRCIC+J2LOCF)))
     *           .LE.(0.001 * ABS(CATR1(KRCIC+J1LOCF)))) .AND.
     *         ((ABS (CATR1(KRCRP+J1LOCF) - CATR2(KRCRP+J2LOCF)))
     *           .LE.0.001) .AND.
     *         ((ABS (CATR1(KRCRT+J1LOCF) - CATR2(KRCRT+J2LOCF)))
     *           .LE.0.001)) GO TO 280
C                                       Tell if frequencies incompatable
                  WRITE (MSGTXT,1270) CATD1(KDCRV+J1LOCF),
     *               CATD2(KDCRV+J2LOCF)
                  IF (.NOT.EQUAL) CALL MSGWRT (8)
                  WRITE (MSGTXT,1271)
                  GO TO 990
C                                        Check sort order, if both the
C                                        same leave as is.
 280  CALL CHCOMP (2, 1, CATH1(KITYP), 1, CATH2(KITYP), EQUAL)
      IF (.NOT.EQUAL) THEN
         CALL CHR2H (4, '**  ', 1, CATH(KITYP))
         END IF
      CALL H2CHR (2, 1, CATH(KITYP), ISORT(1:2))
C                                        Add # vis.
      CATBLK(KIGCN) = CAT1(KIGCN) + CAT2(KIGCN)
C                                       Copy name, class.
      CALL CHR2H (12, NAME3, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS3, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQ3
C                                        Create output file, must be new
      CCNO = 1
      CALL UVCREA (DISK3, CCNO, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 300
         WRITE (MSGTXT,1280) IERR
         GO TO 990
 300  NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK3
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
      SEQ3 = CATBLK(KIIMS)
C                                       copy header keywords
      CALL KEYPCP (FVOL(1), FCNO(1), DISK3, CCNO, 2, KEYSKP, IERR)
      IERR = 0
C                                        Compute order in output record
      LIM = LREC1 - NPARM1
      LIM = LIM / CAT1(KINAX)
      IF (LIM.GT.MAXCIF) THEN
         MSGTXT = 'NUMBER CORRELATORS EXCEEDS MAXCIF: WHAT GIVES?'
         IRET = 16
         GO TO 990
         END IF
      NAXES = CAT1(KIDIM)
C                                        Get output increments
      LIMO = CATBLK(KIDIM) + 1
      INCO(1) = 1
      III = CATBLK(KINAX)
      CATBLK(KINAX) = 1
      DO 310 I = 2,LIMO
         INCO(I) = INCO(I-1) * CATBLK(KINAX+I-2)
 310     CONTINUE
      CATBLK(KINAX) = III
C                                        Get input #1 axis dimensions.
      CALL COPY (KICTPN, CAT1(KINAX), INCI)
      INCI(1) = 1
C                                        Determine orders.
      DO 400 IPASS = 1,2
         DO 350 I = 1,LIM
            II = I - 1
            JORD(IPASS,I) = 0
            DO 340 J = 1,NAXES
               III = MOD (II, INCI(J))
               II = II / INCI (J)
               JP = JPOS(IPASS,J)
               JORD(IPASS,I) = JORD(IPASS,I) + (III*ISGN(J,IPASS) +
     *            IOFF(J,IPASS)) * INCO(JP)
 340           CONTINUE
 350        CONTINUE
C                                        Get input #2.
         IF (IPASS.EQ.1) THEN
            LIM = LREC2 - NPARM2
            NAXES = CAT2(KIDIM)
            CALL COPY (KICTPN, CAT2(KINAX), INCI)
            LIM = LIM / INCI(1)
            INCI(1) = 1
            END IF
 400     CONTINUE
      IRET = 0
C                                       Save reference date
      CALL CHR2H (8, NEWRD, 1, CATH(KHDOB))
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
 999  RETURN
C-----------------------------------------------------------------------
 4000 FORMAT ('Scaling u,v,w of data set',I2,' by',1PE15.7)
 1000 FORMAT ('EPOCHS DO NOT MATCH',2F10.1)
 1070 FORMAT ('NUMBER OF UNLABELED RANDOM PARAMETERS UNEQUAL',2I5)
 1110 FORMAT ('NON MATCHING AXIS TYPE ',A8,' DIMENSION=',I3,' NOT 1')
 1130 FORMAT ('OUTPUT NO. AXES=',I3,' GREATER THAN ',I3)
 1165 FORMAT (A8,' AXIS HAS UNEQUAL ATTRIBUTES')
 1220 FORMAT ('ONLY ONE DATABASE HAS A STOKES AXIS')
 1236 FORMAT ('I CAN ONLY HANDLE STANDARD FORMS OF STOKES AXES')
 1237 FORMAT ('STOKES RANGES DO NOT OVERLAP')
 1265 FORMAT ('ONLY ONE DATABASE HAS A FREQUENCY AXIS')
 1270 FORMAT ('FREQUENCIES DIFFER BY TOO MUCH:',2(1PD16.8))
 1271 FORMAT ('MULTI-FREQUENCY DATA BASES INCOMPATABLE')
 1280 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE DBCOPY (IRET)
C-----------------------------------------------------------------------
C   DBCOPY copies and reformats the two old data bases into a new
C   database.
C   Output: IRET   I    Return error code, 0 => OK, 1 => error, abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   LUN1, LUN2, FIND1, FIND2, I2, IS, J1, IPTRI, IPTRO, IOP,
     *   INIO, NIOUT, BIND1, BIND2, WTOFF, OCNT, INDX, I1M, I2M, I,
     *   J, I1, IRET, IERR, ILENBU, LLREC, NPARM, IPASS, L, IR, IRN,
     *   IBASE, IARR, IU, JOP, VO, BO, SUID, NCMPLX, IROUND, NNCOR,
     *   WWTOFF, TMPCAT(256), NCH, NST, NIF, TABBUF(512), IVER, ILUN,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO, ISBAND(MAXIF,MAXFQC),
     *   NFQID, FID, IIF, IOF, ICH, FOF, IST, NNCORO, IFQ, XLREC,
     *   RNXRET, OREC, IA1, IA2
      LOGICAL   T, F, DOWT, CMPR
      REAL      POLFLX(12), DELFAZ, BASADD(2), TIMADD(2), BASE, ZZ(2),
     *   VV(2), DELFZZ, RRWT, FINC(MAXIF,MAXFQC), REFC, UMAT(3,3), DXC,
     *   DYC, DZC, PMAT(3,3)
      DOUBLE PRECISION TRUEF, REFF, FOFF(MAXIF,MAXFQC)
      CHARACTER IFILE*48, OFILE*48, BNDCOD(MAXIF,MAXFQC)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBCON.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      LOGICAL SOUCHN(MAXSOU)
      INTEGER IMXS
      SAVE SOUCHN
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN1, LUN2, ILUN /16, 17, 18/
      DATA VO, BO /0, 1/
C-----------------------------------------------------------------------
      IRET = 1
C                                       Open first input file
      CALL ZPHFIL ('UV', DISK1, FCNO(1), 1, IFILE, IERR)
      CALL ZOPEN (LUN1, FIND1, DISK1, IFILE, T, F, F, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       Open output file.
      CALL ZPHFIL ('UV', DISK3, FCNO(3), 1, OFILE, IERR)
      CALL ZOPEN (LUN2, FIND2, DISK3, OFILE, T, F, F, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       Reset output CATBLK pointers
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      NCMPLX = CATBLK(KINAX)
      NNCORO = (LREC - NRPARM) / NCMPLX
      XLREC = NRPARM + 3 * NNCORO
C                                       Find weight for compressed data.
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *   IERR)
      CALL AXEFND (8, 'WEIGHT  ', CAT1(KIPCN), CAT1(KHPTP), WWTOFF,
     *   IERR)
C                                       Determine read/write sizes.
      ILENBU = 0
C                                       Init I/O
      CALL UVINIT ('READ', LUN1, FIND1, NVIS1, VO, LREC1, ILENBU,
     *   BUFSZ1, BUFF1, BO, BIND1, IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         GO TO 990
 10   ILENBU = 0
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, ILENBU, BUFSZ2,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INITIALIZ'
         GO TO 990
         END IF
      IPTRO = BIND2
      NIOUT = ILENBU
      OREC = 0
C                                       ignore old NX table
C                                       this fools RNXs to do that
      BASADD(1) = 0.0
      TIMADD(1) = DAYOFF(1)
      IF (DUMC(1).LE.0.0) THEN
         CALL RNXGET (DISK3, FCNO(3), CATBLK)
      ELSE
         CALL DBNXGT (1, TIMADD, BASADD)
         END IF
C                                       make an index table
      CALL RNXINI (DISK3, FCNO(3), CATBLK, RNXRET)
C
      CALL COPY (256, CATBLK, TMPCAT)
      CALL COPY (256, CAT2, CATBLK)
C                                       # channels, Stokes, IFs, in
C                                       second (to be shifted) stream.
      IF (J2LOCF.EQ.-1) THEN
         NCH = 1
      ELSE
         NCH = CAT2(KINAX+J2LOCF)
         END IF
      IF (J2LOCS.EQ.-1) THEN
         NST = 1
      ELSE
         NST = CAT2(KINAX+J2LOCS)
         END IF
      IF (J2LOCI.EQ.-1) THEN
         NIF = 1
      ELSE
         NIF = CAT2(KINAX+J2LOCI)
         END IF
      IF (ISCMP2) THEN
         INCS2 = 3 * INCS2
         INCF2 = 3 * INCF2
         INCIF2 = 3 * INCIF2
         END IF
      REFF    = CATD2(KDCRV+JLOCF)
      REFC    = CATR2(KRCRP+JLOCF)
      IF (DOPOS.GT.0.0) THEN
         CALL FNDEXT ('FQ', CATBLK, IVER)
         IF (IVER.GT.0) THEN
            CALL FQINI ('READ', TABBUF, DISK2, FCNO(2), IVER,
     *         CATBLK, ILUN, IFQRNO, FQKOLS, FQNUMV, NIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1010) IERR
               CALL MSGWRT(2)
               GO TO 990
               END IF
            NFQID = TABBUF(5)
            CALL TABIO ('CLOS', 0, IFQRNO, TABBUF, TABBUF, IERR)
         ELSE
            NFQID = 1
            END IF
         DO 20 FID = 1,NFQID
            IVER = 1
            CALL CHNDAT ('READ', TABBUF, DISK2, FCNO(2), IVER, CATBLK,
     *         ILUN, NIF, FOFF(1,FID), ISBAND(1,FID), FINC(1,FID),
     *         BNDCOD(1,FID), FID, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1011) IERR
               GO TO 990
               END IF
 20         CONTINUE
         END IF
      CALL COPY (256, TMPCAT, CATBLK)
      OCNT = 0
      MAXA(1) = 0
      MAXA(2) = 0
      BASADD(1) = 0.0
      TIMADD(1) = DAYOFF(1)
      LLREC = LREC1
      NPARM = NPARM1
      IPASS = 1
      DXC = 0.0
      DYC = 0.0
      DZC = 0.0
C                                       Weighting
      RRWT = REWT(1)
      NCMPLX = CAT1(KINAX)
      NNCOR = (LLREC - NPARM) / NCMPLX
      DOWT = (ABS (RRWT-1.0).GT.1.0E-3)
      CMPR = ISCMP1
C                                       Init pointers for Stokes
      I1M = 1
      I2M = 1
      DO 25 I = 1,7
         J = I - 1
         L = CATBLK(KINAX+J)
         IF (L.LE.0) GO TO 25
            IF (J.LT.JLOCS) I1M = I1M * L
            IF (J.GT.JLOCS) I2M = I2M * L
 25      CONTINUE
C
      IMXS = MAXSOU
      DO 30 I = 1,IMXS
         SOUCHN(I) = .FALSE.
 30      CONTINUE
C                                       Copy file # 1
C                                       Begin loop
 50   CONTINUE
         CALL UVDISK ('READ', LUN1, FIND1, BUFF1, INIO, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ'
            GO TO 990
            END IF
         IPTRI = BIND1
         IF (INIO.LE.0) GO TO 110
C                                       Loop thru records.
         DO 100 IR = 1,INIO
C                                       Copy record.
            IF (CMPR) THEN
               CALL RCOPY (NPARM, BUFF1(IPTRI,1), TBUFF)
               CALL ZUVXPN  (NNCOR, BUFF1(IPTRI+NPARM,1),
     *            BUFF1(IPTRI+WWTOFF,1), TBUFF(1+NPARM))
            ELSE
               CALL RCOPY (LLREC, BUFF1(IPTRI,1), TBUFF)
               END IF
C                                       Translate Source number
            IF (I2LOCS.GT.0) THEN
               SUID = IROUND (TBUFF(1+I2LOCS))
            ELSE
               SUID = 0
               END IF
            IF (SUID.LT.-32767) SUID = 0
C                                       Translating FQs
            IF ((IPASS.GT.1) .AND. (I2LOCQ.GE.0)) THEN
               IFQ = TBUFF(1+I2LOCQ) + 0.1
               IF ((IFQ.LE.0) .OR. (IFQ.GT.MAXFQ)) IFQ = 1
               IFQ = FQ2TRA(IFQ)
               IF ((IFQ.LE.0) .OR. (IFQ.GT.MAXFQ)) IFQ = 1
               TBUFF(1+I2LOCQ) = IFQ
               END IF
C                                       If translating source IDs
            IF ((IPASS.GT.1) .AND. (NUMSOU.GT.0)) THEN
C                                       If id is actually different
               IF (SUID.NE.SOUTRA(SUID)) THEN
C                                       Change the Source ID
                  TBUFF(1+I2LOCS) = SOUTRA(SUID)
                  IF (.NOT.SOUCHN(SUID)) THEN
                     WRITE(MSGTXT,1050) SUID, SOUTRA(SUID)
                     CALL MSGWRT(3)
                     SOUCHN(SUID) = .TRUE.
                     END IF
                  END IF
               END IF
C                                       Check position shift.
            IF ((IPASS.GT.1) .AND. (DOPOS.GT.0.0)) THEN
               DELFAZ = DXC * TBUFF(1+I2LOCU) + DYC * TBUFF(1+I2LOCV) +
     *            DZC * TBUFF(1+I2LOCW)
               CALL PRJMUL (1, TBUFF(1+I2LOCU), UMAT, TBUFF(1+I2LOCU))
               ZZ(1) = COS (DELFAZ)
               ZZ(2) = -SIN (DELFAZ)
               FID = 1.0
               IF (I2LOCQ.GE.0) FID = INT (TBUFF(1+I2LOCQ))
C                                       Start IF, channel, Stokes loops
               DO 65 IIF = 1,NIF
                  IOF = (IIF - 1) * INCIF2
                  DO 60 ICH = 1,NCH
                     FOF    = (ICH - 1) * INCF2
                     TRUEF  = REFF + (ICH-REFC) * FINC(IIF,FID) +
     *                  FOFF(IIF,FID)
                     DELFZZ = DELFAZ * TRUEF / REFF
                     ZZ(1)  =  COS (DELFZZ)
                     ZZ(2)  = -SIN (DELFZZ)
C                                       DELFAZ corrected; Stokes loop
                     DO 55 IST = 1, NST
                        INDX = 1 + NPARM + FOF + IOF + (IST - 1) * INCS2
                        VV(1) = TBUFF(INDX)
                        VV(2) = TBUFF(INDX+1)
                        TBUFF(INDX) = ZZ(1) * VV(1) - ZZ(2) * VV(2)
                        TBUFF(INDX+1) = ZZ(1) * VV(2) + ZZ(2) * VV(1)
 55                     CONTINUE
 60                  CONTINUE
 65               CONTINUE
               END IF
C                                       Zero output record.
            CALL RFILL (XLREC, 0.0, XBUFF)
C                                       Scale to ref. pixel 1
            IF ((DOUVM1) .AND. (IPASS.EQ.1)) THEN
               TBUFF(1+I1LOCU) = UVMUL1 * TBUFF(1+I1LOCU)
               TBUFF(1+I1LOCV) = UVMUL1 * TBUFF(1+I1LOCV)
               TBUFF(1+I1LOCW) = UVMUL1 * TBUFF(1+I1LOCW)
               END IF
            IF ((DOUVM2) .AND. (IPASS.EQ.2)) THEN
               TBUFF(1+I2LOCU) = UVMUL2 * TBUFF(1+I2LOCU)
               TBUFF(1+I2LOCV) = UVMUL2 * TBUFF(1+I2LOCV)
               TBUFF(1+I2LOCW) = UVMUL2 * TBUFF(1+I2LOCW)
               END IF
C                                       Copy random parameters
            DO 70 IRN = 1,NPARM
               IOP = IORD(IPASS,IRN)
               XBUFF(1+IOP) = TBUFF(IRN)
 70            CONTINUE
C                                       fix new/old forms
            IF ((IPASS.EQ.2) .AND. (DOTRAN.NE.0)) THEN
               IF (DOTRAN.EQ.1) THEN
                  BASE = 256.0 * TBUFF(I2LOC1+1) + TBUFF(I2LOC2+1) +
     *               0.01 * (TBUFF(I2LOCA+1) - 1.0)
                  XBUFF(1+ILOCB) = BASE
               ELSE
                  BASE = TBUFF(I2LOCB+1)
                  IA1 = BASE/256.0 + 0.01
                  IA2 = BASE - 256*IA1 + 0.01
                  IARR = 100.0 * (BASE - 256*IA1 - IA2) + 1.0
                  XBUFF(1+ILOCA1) = IA1
                  XBUFF(1+ILOCA2) = IA2
                  XBUFF(1+ILOCSA) = IARR
                  END IF
               END IF
C                                        Check array numbers.
            IF (ILOCB.GE.0) THEN
               BASE = XBUFF(1+ILOCB)
               IBASE = BASE
               IARR = 100. * (BASE-IBASE) + 0.5
               XBUFF(1+ILOCB) = BASE + BASADD(1)
            ELSE
               IARR = XBUFF(1+ILOCSA) - 0.99
               XBUFF(1+ILOCSA) = XBUFF(1+ILOCSA) + 100.0*BASADD(1)
               END IF
C                                        Adjust time and baseline.
            XBUFF(1+ILOCT) = XBUFF(1+ILOCT) + TIMADD(1)
            MAXA(IPASS) = MAX (MAXA(IPASS), IARR)
C                                       Reweight?
            IF (DOWT) THEN
               DO 74 IU = 1,NNCOR
                  IOP = NPARM + 3 * IU
                  TBUFF(IOP) = TBUFF(IOP) * RRWT
 74               CONTINUE
               END IF
C                                       Copy data.
            DO 75 IU = 1,NNCOR
               IOP = 3 * JORD(IPASS,IU) + 1 + NRPARM
               JOP = NPARM + 3 * IU - 2
               XBUFF(IOP) = TBUFF(JOP)
               XBUFF(IOP+1) = TBUFF(JOP+1)
               XBUFF(IOP+2) = TBUFF(JOP+2)
 75            CONTINUE
C                                       Convert Stokes
            IF (FIXPOL.EQ.IPASS) THEN
               I = 1 + NRPARM - 3
               DO 80 I2 = 1,I2M
               DO 80 IS = 1,4
               DO 80 I1 = 1,I1M,3
                  I = I + 3
                  IF (IS.EQ.1) THEN
                     CALL RFILL (12, 0.0, POLFLX)
C                                       Input -> RR, LL, RL, LR
                     DO 76 J = 1,4
                        IF (POLANT(J).GT.0) THEN
                           J1 = (POLANT(J)-1) * INCS + I
                           CALL RCOPY (3, XBUFF(J1), POLFLX(3*J-2))
                           END IF
 76                     CONTINUE
C                                       RR, LL -> I, V
                     CALL RCOPY (3, POLFLX(1), XBUFF(I))
                     CALL RFILL (3, 0.0, XBUFF(I+3*INCS))
                     IF (POLFLX(6).GT.0.0) THEN
                        IF (POLFLX(3).LE.0.0) THEN
                           CALL RCOPY (3, POLFLX(4), XBUFF(I))
                        ELSE
                           J = I + 3*INCS
                           XBUFF(I+2) = POLFLX(3) + POLFLX(6)
                           XBUFF(I) = (POLFLX(3)*POLFLX(1) + POLFLX(6)*
     *                        POLFLX(4)) / XBUFF(I+2)
                           XBUFF(I+1) = (POLFLX(3)*POLFLX(2) +
     *                        POLFLX(6)*POLFLX(5)) / XBUFF(I+2)
                           XBUFF(J) = (POLFLX(1) - POLFLX(4)) / 2.0
                           XBUFF(J+1) = (POLFLX(2) - POLFLX(5)) / 2.0
                           XBUFF(J+2) = XBUFF(I+2) / 2.0
                           XBUFF(I+2) = XBUFF(J+2)
                           END IF
                        END IF
C                                       RL, LR -> Q, U
                     CALL RFILL (3, 0.0, XBUFF(I+INCS))
                     CALL RFILL (3, 0.0, XBUFF(I+2*INCS))
                     IF ((POLFLX(9).GT.0.0) .AND. (POLFLX(12).GT.0.0))
     *                  THEN
                        J = I + INCS
                        XBUFF(J) = (POLFLX(7) + POLFLX(10)) / 2.0
                        XBUFF(J+1) = (POLFLX(8) + POLFLX(11)) / 2.0
                        XBUFF(J+2) = (POLFLX(9) + POLFLX(12)) / 2.0
                        J = J + INCS
                        XBUFF(J) = (POLFLX(8) - POLFLX(11)) / 2.0
                        XBUFF(J+1) = (POLFLX(10) - POLFLX(7)) / 2.0
                        XBUFF(J+2) = (POLFLX(9) + POLFLX(12)) / 2.0
                        END IF
                     END IF
 80               CONTINUE
               END IF
C                                       update NX table
            CALL RNXUPD (XBUFF, RNXRET)
            OREC = OREC + 1
            IF (MOD(OREC,50000).EQ.0) THEN
               WRITE (MSGTXT,1080) OREC
               CALL MSGWRT (2)
               END IF
C                                       move to output buffer
            IF (ISCMP) THEN
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
               CALL UVDISK ('WRIT', LUN2, FIND2, BUFF2, NIOUT, BIND2,
     *            IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'WRIT'
                  GO TO 990
                  END IF
               OCNT = 0
               IPTRO = BIND2
               END IF
 100        CONTINUE
C                                       Loop back
         GO TO 50
C                                       Close input
 110  CALL ZCLOSE (LUN1, FIND1, IERR)
C                                       If PASS=2 then finished.
      IF (IPASS.EQ.2) GO TO 200
C                                       Setup for second input file.
C                                       Check for no sub array option
         IF (DOARR.GT.0.0) THEN
            TIMADD(1) = DAYOFF(2)
            BASADD(1) = 0.0
         ELSE
            TIMADD(1) = (MAXA(1)+1) * 5.0
            BASADD(1) = (MAXA(1)+1) * 0.01
            END IF
         IF (DUMC(1).GT.0.0) CALL DBNXGT (2, TIMADD, BASADD)
C                                        Determine position shift.
         IF (DOPOS.GT.0.0) THEN
            CALL SHISIN (RA2, DEC2, 0.0, RA1, DEC1, DXC, DYC, DZC)
            CALL PRJMAT (RA2, DEC2, 0.0, RA1, DEC1, 0.0, UMAT, PMAT)
            END IF
         LLREC = LREC2
         NPARM = NPARM2
         IPASS = 2
         CMPR = ISCMP2
C                                       Weighting
         RRWT = REWT(2)
         NCMPLX = CAT2(KINAX)
         NNCOR = (LLREC - NPARM) / NCMPLX
         DOWT = (ABS (RRWT-1.0).GT.1.0E-3)
         CALL AXEFND (8, 'WEIGHT  ', CAT2(KIPCN), CAT2(KHPTP), WWTOFF,
     *      IERR)
C                                       Open second input file
         CALL ZPHFIL ('UV', DISK2, FCNO(2), 1, IFILE, IERR)
         CALL ZOPEN (LUN1, FIND1, DISK2, IFILE, T, F, F, IERR)
         IF (IERR.GT.0) GO TO 999
C                                       Initialize.
         ILENBU = 0
         CALL UVINIT ('READ', LUN1, FIND1, NVIS2, VO, LREC2, ILENBU,
     *      BUFSZ1, BUFF1, BO, BIND1, IERR)
C                                       Loop back and copy 2nd file.
         IF (IERR.EQ.0) GO TO 50
            WRITE (MSGTXT,1000) IERR, 'INITIALIZ'
            GO TO 990
C                                       Flush and close files.
 200  NIOUT = -OCNT
      CALL UVDISK ('FLSH', LUN2, FIND2, BUFF2, NIOUT, BIND2, IERR)
      IF (IERR.EQ.0) GO TO 210
         WRITE (MSGTXT,1000) IERR, 'FLUSH'
         GO TO 990
C                                       Close
 210  CALL ZCLOSE (LUN2, FIND2, IERR)
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
 1000 FORMAT ('ERROR',I3,1X,A,'ING UV FILE')
 1010 FORMAT ('DBCOPY: ERROR ',I3,' ACCESSING FQ TABLE')
 1011 FORMAT ('DBCOPY: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1050 FORMAT ('DBCOPY: changing source ID',I5,' to',I5)
 1080 FORMAT ('DBCOPY: at output record',I10)
      END
      SUBROUTINE DBMNGL (IRET)
C-----------------------------------------------------------------------
C   DBMNGL copies and reformats the two old data bases into a new
C   database merging the records to preserve the common sort order.
C   Output: IRET   I    Return error code, 0 => OK, 1 => error, abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SCODE(2),LUN1(2), LUN2, ILUN, FIND1(2), FIND2, IPTRI(2),
     *   IPTRO, ILENBU, INIO(2), INCNT(2), IU, NIOUT, BIND1(2), BIND2,
     *   OCNT, STREAM, I, J, I1, I2, IS, J1, IC, LIM(2), I1M, I2M, JOP,
     *   L, BO, VO, MCMPLX(2), INDX, JNDX, JJLOCB(2), JJLOCT(2), IFQ,
     *   LLREC(2), NPARM(2), IRET, IERR, IRN, IOP, SUID, WTOFF, WOFF(2),
     *   IROUND, ICH, NCH, IST, NST, IIF, NIF, INDX0, FOF, IOF, FID,
     *   NSHIFT, TMPCAT(256), IVER, TABBUF(512), NFQID, NNCOR, NNPARM,
     *   FQKOLS(MAXFQC), FQNUMV(MAXFQC), IFQRNO, ISBAND(MAXIF,MAXFQC),
     *   IMXS, NNCORO, NCMPLX, XLREC, LNCS, RNXRET, OREC, JJLOCA(2),
     *   IA1, IA2, IARR
      LOGICAL   T, F, EOD(2), DOWT(2), FIRST
      REAL      POLFLX(12), FINC(MAXIF,MAXFQC), ZZ(2), KEY(2,2), REFC,
     *   BASADD(2), TIMADD(2), VV(2), RRWT, DELFAZ, DELFZZ, UMAT(3,3),
     *   PMAT(3,3), DXC, DYC, DZC, BASE
      DOUBLE PRECISION TRUEF, REFF, FOFF(MAXIF,MAXFQC)
      CHARACTER IFILE(2)*48, OFILE*48, BNDCOD(MAXIF,MAXFQC)*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBCON.INC'
      LOGICAL   SOUCHN(MAXSOU)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE SOUCHN
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN1, LUN2, ILUN /16, 17, 18, 50/
      DATA VO, BO /0, 1/
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IRET = 1
C                                       Open first input file
      CALL ZPHFIL ('UV', DISK1, FCNO(1), 1, IFILE(1), IERR)
      CALL ZOPEN (LUN1(1), FIND1(1), DISK1, IFILE(1), T, F, F, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       Open second input file
      CALL ZPHFIL ('UV', DISK2, FCNO(2), 1, IFILE(2), IERR)
      CALL ZOPEN (LUN1(2), FIND1(2), DISK2, IFILE(2), T, F, F, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       Open output file.
      CALL ZPHFIL ('UV', DISK3, FCNO(3), 1, OFILE, IERR)
      CALL ZOPEN (LUN2, FIND2, DISK3, OFILE, T, F, F, IERR)
      IF (IERR.GT.0) GO TO 999
C                                       Reset output CATBLK pointers
      CALL UVPGET (IERR)
      IF (IERR.NE.0) GO TO 999
      NCMPLX = CATBLK(KINAX)
      NNCORO = (LREC - NRPARM) / NCMPLX
      XLREC = NRPARM + 3 * NNCORO
C                                       Find weight for compressed data.
      CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *   IERR)
      CALL AXEFND (8, 'WEIGHT  ', CAT1(KIPCN), CATH1(KHPTP), WOFF(1),
     *   IERR)
      CALL AXEFND (8, 'WEIGHT  ', CAT2(KIPCN), CATH2(KHPTP), WOFF(2),
     *   IERR)
C                                       Weighting
      MCMPLX(1) = CAT1(KINAX)
      DOWT(1) = ABS(REWT(1)-1.0).GT.1.0E-3
      MCMPLX(2) = CAT2(KINAX)
      DOWT(2) = ABS(REWT(2)-1.0).GT.1.0E-3
C                                       Determine read/write sizes.
      ILENBU = 0
C                                       Init I/O
C                                       First input file.
      CALL UVINIT ('READ', LUN1(1), FIND1(1), NVIS1, VO, LREC1, ILENBU,
     *   BUFSZ1, BUFF1(1,1), BO, BIND1(1), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT READ', 'INPUT 1'
         GO TO 990
         END IF
C                                       Second input file.
      ILENBU = 0
      CALL UVINIT ('READ', LUN1(2), FIND1(2), NVIS2, VO, LREC2, ILENBU,
     *   BUFSZ1, BUFF1(1,2), BO, BIND1(2), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT READ', 'INPUT 2'
         GO TO 990
         END IF
      ILENBU = 0
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, ILENBU, BUFSZ2,
     *   BUFF2, BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'INIT WRIT', 'OUTPUT'
         GO TO 990
         END IF
      IPTRO = BIND2
      NIOUT = ILENBU
      OCNT = 0
      OREC = 0
C
      IMXS = MAXSOU
      DO 35 I = 1,IMXS
         SOUCHN(I) = .FALSE.
 35      CONTINUE
C                                       Find number of sub arrays in
C                                       each file.
      CALL FNDEXT ('AN', CAT1, MAXA(1))
      CALL FNDEXT ('AN', CAT2, MAXA(2))
      MAXA(1) = MAXA(1) - 1
      MAXA(2) = MAXA(2) - 1
C                                       Crunch sort order
      CALL DBSORD (ISORT, SCODE)
C                                       Set stream dependent parameters.
      BASADD(1) = 0.0
      TIMADD(1) = DAYOFF(1)
      LLREC(1) = LREC1
      NPARM(1) = NPARM1
      LIM(1) = LLREC(1) - NPARM(1)
      JJLOCT(1) = I1LOCT
      JJLOCB(1) = I1LOCB
      JJLOCA(1) = I1LOCA
C                                       Setup for second input file.
C                                       Check for no sub array option
      IF (DOARR.GT.0.0) THEN
         TIMADD(2) = DAYOFF(2)
         BASADD(2) = 0.0
      ELSE
         TIMADD(2) = (MAXA(1)+1) * 5.0
         BASADD(2) = (MAXA(1)+1) * 0.01
         END IF
C                                        Determine position shift.
      IF (DOPOS.GT.0.0) THEN
         CALL SHISIN (RA2, DEC2, 0.0, RA1, DEC1, DXC, DYC, DZC)
         CALL PRJMAT (RA2, DEC2, 0.0, RA1, DEC1, 0.0, UMAT, PMAT)
         END IF
      LLREC(2) = LREC2
      NPARM(2) = NPARM2
      LIM(2) = LLREC(2) - NPARM(2)
      JJLOCT(2) = I2LOCT
      JJLOCB(2) = I2LOCB
      JJLOCA(2) = I2LOCA
      EOD(1) = F
      EOD(2) = F
C                                       ignore old NX table
C                                       this fools RNXs to do that
      IF (DUMC(1).LE.0.0) THEN
         CALL RNXGET (DISK3, FCNO(3), CATBLK)
      ELSE
         CALL DBNXGT (0, TIMADD, BASADD)
         END IF
C                                       make an index table
      CALL RNXINI (DISK3, FCNO(3), CATBLK, RNXRET)
C                                       Init pointers for Stokes
      I1M = 1
      I2M = 1
      DO 50 I = 1,7
         J = I - 1
         L = CATBLK(KINAX+J)
         IF ((J.EQ.1) .AND. (L.EQ.1)) L = 3
         IF (L.GT.0) THEN
            IF (J.LT.JLOCS) I1M = I1M * L
            IF (J.GT.JLOCS) I2M = I2M * L
            END IF
 50      CONTINUE
      INCNT(1) = 30000
      INCNT(2) = 30000
      INIO(1) = 0
      INIO(2) = 0
      NSHIFT  = 0
      LNCS = INCS
      IF (ISCMP) LNCS = INCS * 3
C                                       # channels, Stokes, IFs, in
C                                       second (to be shifted) stream.
      IF (J2LOCF.EQ.-1) THEN
         NCH = 1
      ELSE
         NCH = CAT2(KINAX+J2LOCF)
         END IF
      IF (J2LOCS.EQ.-1) THEN
         NST = 1
      ELSE
         NST = CAT2(KINAX+J2LOCS)
         END IF
      IF (J2LOCI.EQ.-1) THEN
         NIF = 1
      ELSE
         NIF = CAT2(KINAX+J2LOCI)
         END IF
      IF (ISCMP2) THEN
         INCS2 = 3 * INCS2
         INCF2 = 3 * INCF2
         INCIF2 = 3 * INCIF2
         END IF
      REFF = CATD2(KDCRV+JLOCF)
      REFC = CATR2(KRCRP+JLOCF)
C                                       temporarily use CATBLK for CAT2
      CALL COPY (256, CATBLK, TMPCAT)
      CALL COPY (256, CAT2, CATBLK)
      IF (DOPOS.GT.0) THEN
         CALL FNDEXT ('FQ', CATBLK, IVER)
         IF (IVER.GT.0) THEN
            CALL FQINI ('READ', TABBUF, DISK2, FCNO(2), IVER, CATBLK,
     *         ILUN, IFQRNO, FQKOLS, FQNUMV, NIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE(MSGTXT,1040) IERR
               CALL MSGWRT(2)
               GO TO 990
               END IF
            NFQID = TABBUF(5)
            CALL TABIO ('CLOS', 0, IFQRNO, TABBUF, TABBUF, IERR)
         ELSE
            NFQID = 1
            END IF
         DO 55 FID = 1,NFQID
            IVER = 1
            CALL CHNDAT ('READ', TABBUF, DISK2, FCNO(2), IVER, CATBLK,
     *         ILUN, NIF, FOFF(1,FID), ISBAND(1,FID), FINC(1,FID),
     *         BNDCOD(1,FID), FID, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1050) IERR
               GO TO 990
               END IF
 55         CONTINUE
         END IF
C
 60   CONTINUE
C                                       For both streams
         DO 80 IC = 1,2
C                                       If time to read data
            IF ((INCNT(IC).GT.INIO(IC)) .AND. (.NOT. EOD(IC))) THEN
               CALL UVDISK ('READ', LUN1(IC), FIND1(IC), BUFF1(1,IC),
     *            INIO(IC), BIND1(IC), IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ', 'INPUT'
                  GO TO 990
                  END IF
C                                       Keep track of pointers & counts
               IPTRI(IC) = BIND1(IC)
               INCNT(IC) = 1
C                                       Check if out of data this stream
               EOD(IC) = INIO(IC).LE.0
C                                       If not out of data
               IF (.NOT.EOD(IC)) THEN
C                                       Update time and baseline.
                  INDX = IPTRI(IC)
                  JNDX = JJLOCT(IC)
C                                       Add extra days as needed
                  BUFF1(INDX+JNDX,IC) = BUFF1(INDX+JNDX,IC) + TIMADD(IC)
                  JNDX = JJLOCB(IC)
                  IF (JNDX.GE.0) THEN
                     BUFF1(INDX+JNDX,IC) = BUFF1(INDX+JNDX,IC) +
     *                  BASADD(IC)
                  ELSE
                     JNDX = JJLOCA(IC)
                     BUFF1(INDX+JNDX,IC) = BUFF1(INDX+JNDX,IC) +
     *                  BASADD(IC) * 100.0
                     END IF
C                                       Set keys
                  CALL DBKEY (SCODE, IC, BUFF1(INDX,IC), KEY(1,IC))
                  END IF
               END IF
 80         CONTINUE
C                                       Check if out of data
         IF ((EOD(1)) .AND. (EOD(2))) THEN
            GO TO 200
C                                       Pick next record.
         ELSE IF (EOD(2)) THEN
            STREAM = 1
         ELSE IF (EOD(1)) THEN
            STREAM = 2
         ELSE IF (KEY(1,1).GT.KEY(1,2)) THEN
            STREAM = 1
         ELSE IF (KEY(1,1).LT.KEY(1,2)) THEN
            STREAM = 2
         ELSE IF (KEY(2,1).GE.KEY(2,2)) THEN
            STREAM = 1
         ELSE
            STREAM = 2
            END IF
C                                       Copy record.
         INDX = IPTRI(STREAM)
         NNCOR = LIM(STREAM) / MCMPLX(STREAM)
         NNPARM = NPARM(STREAM)
C                                       Copy record.
         IF (MCMPLX(STREAM).EQ.1) THEN
            CALL RCOPY (NNPARM, BUFF1(INDX,STREAM), TBUFF)
            CALL ZUVXPN (NNCOR, BUFF1(INDX+NNPARM,STREAM),
     *         BUFF1(INDX+WOFF(STREAM),STREAM), TBUFF(1+NNPARM))
         ELSE
            CALL RCOPY (LLREC(STREAM), BUFF1(INDX,STREAM), TBUFF)
            END IF
C                                       Translating FQs
         IF ((STREAM.GT.1) .AND. (I2LOCQ.GE.0)) THEN
            IFQ = TBUFF(1+I2LOCQ) + 0.1
            IF ((IFQ.LE.0) .OR. (IFQ.GT.MAXFQ)) IFQ = 1
            IFQ = FQ2TRA(IFQ)
            IF ((IFQ.LE.0) .OR. (IFQ.GT.MAXFQ)) IFQ = 1
            TBUFF(1+I2LOCQ) = IFQ
            END IF
C                                       If Processing second stream
C                                       and a multi source data set
         IF ((STREAM.GT.1) .AND. (NUMSOU.GT.0)) THEN
            IF (I2LOCS.GT.0) THEN
               SUID = IROUND (TBUFF(1+I2LOCS))
            ELSE
               SUID = 0
               END IF
C                                       Change source id
            IF (SUID.NE.SOUTRA(SUID)) THEN
               TBUFF(1+I2LOCS) = SOUTRA(SUID)
               IF (.NOT.SOUCHN(SUID)) THEN
                  WRITE (MSGTXT,1100) SUID, SOUTRA(SUID)
                  CALL MSGWRT (3)
                  SOUCHN(SUID) = .TRUE.
                  END IF
                END IF
            END IF
C                                       If shifting second stream
         IF ((STREAM.NE.1) .AND. (DOPOS.GT.0.0)) THEN
            INDX0  = 1 + NNPARM
            DELFAZ = DXC * TBUFF(1+I2LOCU) + DYC * TBUFF(1+I2LOCV) +
     *         DZC * TBUFF(1+I2LOCW)
            CALL PRJMUL (1, TBUFF(1+I2LOCU), UMAT, TBUFF(1+I2LOCU))
            FID = 1.0
            IF (I2LOCQ.GE.0) FID = INT (TBUFF(1+I2LOCQ))
C                                       Start IF, channel, Stokes loops
            DO 114 IIF = 1,NIF
               IOF = (IIF - 1) * INCIF2
               DO 112 ICH = 1,NCH
                  FOF = (ICH - 1) * INCF2
                  TRUEF  = REFF + (ICH-REFC) * FINC(IIF,FID) +
     *               FOFF(IIF,FID)
                  DELFZZ = DELFAZ * TRUEF / REFF
                  ZZ(1)  =  COS (DELFZZ)
                  ZZ(2)  = -SIN (DELFZZ)
                  DO 110 IST = 1, NST
                     INDX = INDX0 + FOF + IOF + (IST - 1) * INCS2
                     JNDX = INDX + 1
                     VV(1) = TBUFF(INDX)
                     VV(2) = TBUFF(JNDX)
                     TBUFF(INDX) = ZZ(1) * VV(1) - ZZ(2) * VV(2)
                     TBUFF(JNDX) = ZZ(1) * VV(2) + ZZ(2) * VV(1)
 110                 CONTINUE
 112              CONTINUE
 114           CONTINUE
            END IF
C                                       Zero output record.
         CALL RFILL (LREC, 0.0, BUFF2(IPTRO))
C                                       Scale to ref. pixel 1
         IF ((DOUVM1) .AND. (STREAM.EQ.1)) THEN
            TBUFF(1+I1LOCU) = UVMUL1 * TBUFF(1+I1LOCU)
            TBUFF(1+I1LOCV) = UVMUL1 * TBUFF(1+I1LOCV)
            TBUFF(1+I1LOCW) = UVMUL1 * TBUFF(1+I1LOCW)
            END IF
         IF ((DOUVM2) .AND. (STREAM.EQ.2)) THEN
            TBUFF(1+I2LOCU) = UVMUL2 * TBUFF(1+I2LOCU)
            TBUFF(1+I2LOCV) = UVMUL2 * TBUFF(1+I2LOCV)
            TBUFF(1+I2LOCW) = UVMUL2 * TBUFF(1+I2LOCW)
            END IF
C                                       Zero output record.
         CALL RFILL (XLREC, 0.0, XBUFF)
C                                       Copy random parameters
         DO 130 IRN = 1,NNPARM
            IOP = IORD(STREAM,IRN)
            XBUFF(1+IOP) = TBUFF(IRN)
 130        CONTINUE
C                                       fix new/old forms
         IF ((STREAM.EQ.2) .AND. (DOTRAN.NE.0)) THEN
            IF (DOTRAN.EQ.1) THEN
               BASE = 256.0 * TBUFF(I2LOC1+1) + TBUFF(I2LOC2+1) +
     *            0.01 * (TBUFF(I2LOCA+1) - 1.0)
               XBUFF(1+ILOCB) = BASE
            ELSE
               BASE = TBUFF(I2LOCB+1)
               IA1 = BASE/256.0 + 0.01
               IA2 = BASE - 256*IA1 + 0.01
               IARR = 100.0 * (BASE - 256*IA1 - IA2) + 1.0
               XBUFF(1+ILOCA1) = IA1
               XBUFF(1+ILOCA2) = IA2
               XBUFF(1+ILOCSA) = IARR
               END IF
            END IF
         IF (DOWT(STREAM)) THEN
            RRWT = REWT(STREAM)
            DO 135 IU = 1,NNCOR
               JOP = NNPARM + 3 * IU
               TBUFF(JOP) = TBUFF(JOP) * RRWT
 135           CONTINUE
            END IF
C                                        Copy data.
         DO 140 IU = 1,NNCOR
            IOP = 3 * JORD(STREAM,IU) + 1 + NRPARM
            JOP = NPARM(STREAM) + 3 * IU - 2
            XBUFF(IOP) = TBUFF(JOP)
            XBUFF(IOP+1) = TBUFF(JOP+1)
            XBUFF(IOP+2) = TBUFF(JOP+2)
 140        CONTINUE
C                                       Convert Stokes
         IF (FIXPOL.EQ.STREAM) THEN
            I = 1 + NRPARM - 3
            DO 180 I2 = 1,I2M
               DO 175 IS = 1,4
                  DO 170 I1 = 1,I1M,3
                     I = I + 3
                     IF (IS.EQ.1) THEN
                        CALL RFILL (12, 0.0, POLFLX)
C                                       Input -> RR, LL, RL, LR
                        DO 150 J = 1,4
                           IF (POLANT(J).GT.0) THEN
                              J1 = (POLANT(J)-1) * LNCS + I
                              CALL RCOPY (3, XBUFF(J1), POLFLX(3*J-2))
                              END IF
 150                       CONTINUE
C                                       RR, LL -> I, V
                        CALL RCOPY (3, POLFLX(1), XBUFF(I))
                        CALL RFILL (3, 0.0, XBUFF(I+3*LNCS))
                        IF (POLFLX(6).GT.0.0) THEN
                           IF (POLFLX(3).LE.0.0) THEN
                              CALL RCOPY (3, POLFLX(4), XBUFF(I))
                           ELSE
                              J = I + 3*LNCS
                              XBUFF(I+2) = POLFLX(3) + POLFLX(6)
                              XBUFF(I) = (POLFLX(1) + POLFLX(4)) / 2.0
                              XBUFF(I+1) = (POLFLX(2) + POLFLX(5)) / 2.0
                              XBUFF(J) = (POLFLX(1) - POLFLX(4)) / 2.0
                              XBUFF(J+1) = (POLFLX(2) - POLFLX(5)) / 2.0
                              XBUFF(J+2) = 4.0 * POLFLX(3) * POLFLX(6) /
     *                           (POLFLX(3) + POLFLX(6))
                              XBUFF(I+2) = XBUFF(J+2)
                              END IF
                           END IF
C                                       RL, LR -> Q, U
                        CALL RFILL (3, 0.0, XBUFF(I+LNCS))
                        CALL RFILL (3, 0.0, XBUFF(I+2*LNCS))
                        IF ((POLFLX(9).GT.0.0) .AND.
     *                     (POLFLX(12).GT.0.0)) THEN
                           J = I + LNCS
                           XBUFF(J) = (POLFLX(7) + POLFLX(10)) / 2.0
                           XBUFF(J+1) = (POLFLX(8) + POLFLX(11)) / 2.0
                           XBUFF(J+2) = (POLFLX(9) + POLFLX(12)) / 2.0
                           J = J + LNCS
                           XBUFF(J) = (POLFLX(8) - POLFLX(11)) / 2.0
                           XBUFF(J+1) = (POLFLX(10) - POLFLX(7)) / 2.0
                           XBUFF(J+2) = (POLFLX(9) + POLFLX(12)) / 2.0
                           END IF
                        END IF
 170                 CONTINUE
 175              CONTINUE
 180           CONTINUE
            FIRST = .FALSE.
            END IF
C                                       update NX table
         CALL RNXUPD (XBUFF, RNXRET)
         OREC = OREC + 1
         IF (MOD(OREC,50000).EQ.0) THEN
            WRITE (MSGTXT,1180) OREC
            CALL MSGWRT (2)
            END IF
C                                       move to output buffer
         IF (ISCMP) THEN
            CALL RCOPY (NRPARM, XBUFF, BUFF2(IPTRO))
            CALL ZUVPAK (NNCORO, XBUFF(1+NRPARM), BUFF2(IPTRO+WTOFF),
     *         BUFF2(IPTRO+NRPARM))
         ELSE
            CALL RCOPY (LREC, XBUFF, BUFF2(IPTRO))
            END IF
C                                       Update pointers.
         OCNT = OCNT + 1
         IPTRO = IPTRO + LREC
         IPTRI(STREAM) = IPTRI(STREAM) + LLREC(STREAM)
         INCNT(STREAM) = INCNT(STREAM) + 1
C                                       Get new key unless buffer
C                                       exhausted
         IF (INCNT(STREAM).LE.INIO(STREAM)) THEN
C                                       Update time and baseline.
            INDX = IPTRI(STREAM)
            JNDX = JJLOCT(STREAM)
            BUFF1(INDX+JNDX,STREAM) = BUFF1(INDX+JNDX,STREAM) +
     *         TIMADD(STREAM)
            JNDX = JJLOCB(STREAM)
            IF (JNDX.GE.0) THEN
               BUFF1(INDX+JNDX,STREAM) = BUFF1(INDX+JNDX,STREAM) +
     *            BASADD(STREAM)
            ELSE
               JNDX = JJLOCA(STREAM)
               BUFF1(INDX+JNDX,STREAM) = BUFF1(INDX+JNDX,STREAM) +
     *            BASADD(STREAM) * 100.0
               END IF
C                                       New key.
            CALL DBKEY (SCODE, STREAM, BUFF1(INDX,STREAM),
     *         KEY(1,STREAM))
            END IF
C                                       Write if necessary
         IF (OCNT.GE.NIOUT) THEN
            NIOUT = OCNT
            CALL UVDISK ('WRIT', LUN2, FIND2, BUFF2, NIOUT, BIND2,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'WRIT', 'OUTPUT'
               GO TO 990
               END IF
            OCNT = 0
            IPTRO = BIND2
            END IF
C                                       Loop back
         GO TO 60
C                                       Reconstruct CATBLK, flush&close
 200  CALL COPY (256, TMPCAT, CATBLK)
      NIOUT = -OCNT
      CALL UVDISK ('FLSH', LUN2, FIND2, BUFF2, NIOUT, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'FLSH', 'OUTPUT;'
         GO TO 990
         END IF
C                                       Close
      CALL ZCLOSE (LUN2, FIND2, IERR)
      CALL ZCLOSE (LUN1(1), FIND1(1), IERR)
      CALL ZCLOSE (LUN1(2), FIND1(2), IERR)
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
 1000 FORMAT ('ERROR',I3,1X,A,'ING ',A,' FILE')
 1040 FORMAT ('DBMNGL: error ',I3,' Accessing FQ table')
 1050 FORMAT ('DBMNGL: ERROR',I3,' GETTING FREQ. INFO. WITH CHNDAT')
 1100 FORMAT ('DBMNGL: changing source ID',I5,' to',I5)
 1180 FORMAT ('DBMNGL: at output record',I10)
      END
      SUBROUTINE DBNXGT (NUMFIL, OFFD, OFFA)
C-----------------------------------------------------------------------
C   Init the time breaks in the index table info
C   Inputs:
C      NUMFIL   I      File number to do, 0 -> both
C      OFFD     R(2)   Day number offset to apply
C      OFFA     R(2)   Subarray number offset to apply (* 0.01)
C                         (NUMFIL=1,2 use OFFD(1), OFFA(1))
C   Outputs:
C      DRNX.INC common
C-----------------------------------------------------------------------
      INTEGER   NUMFIL
      REAL      OFFD(*), OFFA(*)
C
      INCLUDE 'DBCON.INC'
      INTEGER   AOFF(2), NSUB(2), LUN, LUNTMP, JS, I, J, NV, NP, VER,
     *   ISUB, NS, IERR, JJ
      REAL      DOFF(2), TMIN
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'INCS:DRNX.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      LUN = LUNTMP (1)
C                                       subarrays
      CALL FNDEXT ('AN', CAT1, NSUB(1))
      IF (NUMFIL.EQ.2) NSUB(1) = 0
      CALL FNDEXT ('AN', CAT2, NSUB(2))
      IF (NUMFIL.EQ.1) NSUB(2) = 0
      VER = 1
      IF (NSUB(1).GT.0) THEN
         CALL ISTAB ('NX', DISK1, FCNO(1), VER, LUN, RNXBUF, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((IERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) NSUB(1)=0
         END IF
      IF (NSUB(2).GT.0) THEN
         CALL ISTAB ('NX', DISK2, FCNO(2), VER, LUN, RNXBUF, TABLE,
     *      EXIST, FITASC, IERR)
         IF ((IERR.NE.0) .OR. (.NOT.TABLE) .OR. (.NOT.EXIST)) NSUB(2)=0
         END IF
      IF (NUMFIL.EQ.0) THEN
         AOFF(1) = 100.0 * OFFA(1) + 0.01
         AOFF(2) = 100.0 * OFFA(2) + 0.01
         DOFF(1) = OFFD(1)
         DOFF(2) = OFFD(2)
      ELSE
         AOFF(NUMFIL) = 100.0 * OFFA(1) + 0.01
         DOFF(NUMFIL) = OFFD(1)
         END IF
C                                       no data
      IF ((NSUB(1).EQ.0) .AND. (NSUB(2).EQ.0)) THEN
         CALL FILL (MAXSUB, 0, RNXNOS)
C                                       Only first
      ELSE IF (NSUB(2).LE.0) THEN
         DO 30 ISUB = 1,NSUB(1)
            JS = ISUB + AOFF(1)
            IF (ISUB.EQ.1) THEN
               RNXFIR(JS) = 1
            ELSE
               RNXFIR(JS) = RNXFIR(JS-1) + RNXNOS(JS-1)
               END IF
            CALL GETNX (LUN, DISK1, FCNO(1), CAT1, ISUB, RNXBUF,
     *         RNXNOS(JS), RNXTSC(RNXFIR(JS)))
            DO 20 I = 1,RNXNOS(JS)
               J = I + RNXFIR(JS) - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(1)
 20            CONTINUE
 30         CONTINUE
C                                       Only second
      ELSE IF (NSUB(1).LE.0) THEN
         DO 50 ISUB = 1,NSUB(2)
            JS = ISUB + AOFF(2)
            IF (ISUB.EQ.1) THEN
               RNXFIR(JS) = 1
            ELSE
               RNXFIR(JS) = RNXFIR(JS-1) + RNXNOS(JS-1)
               END IF
            CALL GETNX (LUN, DISK2, FCNO(2), CAT2, ISUB, RNXBUF,
     *         RNXNOS(JS), RNXTSC(RNXFIR(JS)))
            DO 40 I = 1,RNXNOS(JS)
               J = I + RNXFIR(JS) - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(1)
 40            CONTINUE
 50         CONTINUE
C                                       simple
      ELSE IF (1+AOFF(2).GT.NSUB(1)+AOFF(1)) THEN
         DO 70 ISUB = 1,NSUB(1)
            JS = ISUB + AOFF(1)
            IF (ISUB.EQ.1) THEN
               RNXFIR(JS) = 1
            ELSE
               RNXFIR(JS) = RNXFIR(JS-1) + RNXNOS(JS-1)
               END IF
            CALL GETNX (LUN, DISK1, FCNO(1), CAT1, ISUB, RNXBUF,
     *         RNXNOS(JS), RNXTSC(RNXFIR(JS)))
            DO 60 I = 1,RNXNOS(JS)
               J = I + RNXFIR(JS) - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(1)
 60            CONTINUE
 70         CONTINUE
         DO 90 ISUB = 1,NSUB(2)
            JS = ISUB + AOFF(2)
            IF (ISUB.EQ.1) THEN
               RNXFIR(JS) = RNXFIR(NSUB(1)+AOFF(1)) +
     *            RNXNOS(NSUB(1)+AOFF(1))
            ELSE
               RNXFIR(JS) = RNXFIR(JS-1) + RNXNOS(JS-1)
               END IF
            CALL GETNX (LUN, DISK2, FCNO(2), CAT2, ISUB, RNXBUF,
     *         RNXNOS(JS), RNXTSC(RNXFIR(JS)))
            DO 80 I = 1,RNXNOS(JS)
               J = I + RNXFIR(JS) - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(2)
 80            CONTINUE
 90         CONTINUE
C                                       mixed subarrays
      ELSE
         NS = MAX (NSUB(1), NSUB(2))
         DO 150 ISUB = 1,NS
            JS = ISUB
            IF (ISUB.EQ.1) THEN
               RNXFIR(JS) = 1
            ELSE
               RNXFIR(JS) = RNXFIR(JS-1) + RNXNOS(JS-1)
               END IF
            CALL GETNX (LUN, DISK1, FCNO(1), CAT1, ISUB, RNXBUF,
     *         RNXNOS(JS), RNXTSC(RNXFIR(JS)))
            DO 110 I = 1,RNXNOS(JS)
               J = I + RNXFIR(JS) - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(1)
 110           CONTINUE
            NV = 0
            NP = RNXFIR(JS) + RNXNOS(JS)
            CALL GETNX (LUN, DISK2, FCNO(2), CAT2, ISUB, RNXBUF,
     *         NV, RNXTSC(NP))
            DO 120 I = 1,NV
               J = I + NP - 1
               RNXTSC(J) = RNXTSC(J) + DOFF(2)
 120           CONTINUE
            RNXNOS(JS) = RNXNOS(JS) + NV
C                                       sort in time order
            NV = RNXNOS(JS)
            NP = RNXFIR(JS) - 1
            DO 140 I = 1,NV-1
               TMIN = 999999.
               DO 130 J = I,NV
                  IF (RNXTSC(J+NP).LT.TMIN) THEN
                     TMIN = RNXTSC(J+NP)
                     JJ = J
                     END IF
 130              CONTINUE
               RNXTSC(JJ+NP) = RNXTSC(I+NP)
               RNXTSC(I+NP) = TMIN
 140           CONTINUE
 150        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE DBSORD (SORT, SCODE)
C-----------------------------------------------------------------------
C   DBSORD determines a sort order code for the first and second keys
C   of a two key sort order (SORT).  An unrecognized code will be
C   considered '**' (unsorted).
C    Input:  SORT     C*2   Two character sort code
C    Output:
C            SCODE(2) I    Sort codes.
C                        'B'=>1, 'T'=>2, 'U'=>3, 'V'=>4, 'W'=>5,
C                        'R'=>6, 'P'=>7, 'X'=>8, 'Y'=>9, 'Z'=>10,
C                        'M'=>11, '*'=>12
C-----------------------------------------------------------------------
      CHARACTER CODES(12)*1, SORT*2
      INTEGER   SCODE(2), NCODES, I
      DATA CODES /'B','T','U','V','W','R','P','X','Y','Z','M','*'/
      DATA NCODES /12/
C-----------------------------------------------------------------------
C                                       Set default ('**')
      SCODE(1) = 12
      SCODE(2) = 12
      DO 10 I = 1, NCODES
C                                       First code.
         IF (CODES(I).EQ.SORT(1:1)) SCODE(1) = I
C                                       Second code.
         IF (CODES(I).EQ.SORT(2:2)) SCODE(2) = I
 10      CONTINUE
      IF (SCODE(1).EQ.12) SORT(1:1) = '*'
      IF (SCODE(2).EQ.12) SORT(2:2) = '*'
 999  RETURN
      END
      SUBROUTINE DBKEY (SCODE, STREAM, RECORD, KEY)
C-----------------------------------------------------------------------
C   DBKEY determines the sort keys from a record
C    Inputs:
C     SCODE(2)  I    Sort codes for two keys (see DBSORD for details)
C     STREAM    I    Stream number, 1=>first, 2=>second.
C     RECORD(*) R    Record.  Indices from CDBC.INC common.
C    Outputs:
C     KEY(2)    R    Key values
C-----------------------------------------------------------------------
      INTEGER   SCODE(2), STREAM, IKEY
      REAL      RECORD(10), KEY(2), XX, YY
      INCLUDE 'DBCON.INC'
C-----------------------------------------------------------------------
C                                       Decide which stream
      IF (STREAM.EQ.2) GO TO 150
C                                       Loop
      DO 140 IKEY = 1,2
C                                       Order by descending values of
C                                       keys,  negate keys.
         GO TO (20, 30, 40, 50, 60, 70, 80, 90, 100,
     *      110, 120, 130), SCODE(IKEY)
C                                       Baseline number.
 20         KEY(IKEY) = -RECORD(I1LOCB+1)
            GO TO 140
C                                       Time.
 30         KEY(IKEY) = -RECORD(I1LOCT+1)
            GO TO 140
C                                       U
 40         KEY(IKEY) = -RECORD(I1LOCU+1)
            GO TO 140
C                                       V
 50         KEY(IKEY) = -RECORD(I1LOCV+1)
            GO TO 140
C                                       W
 60         KEY(IKEY) = -RECORD(I1LOCW+1)
            GO TO 140
C                                       Baseline length.
 70         XX = RECORD(I1LOCU+1)
            YY = RECORD(I1LOCV+1)
            KEY(IKEY) = -SQRT (XX*XX + YY*YY)
            GO TO 140
C                                       Baseline PA
 80         XX = RECORD(I1LOCU+1)
            YY = RECORD(I1LOCV+1)
            KEY(IKEY) = -ATAN2 (YY, XX+1.0E-20)
            GO TO 140
C                                       Descending ABS(u)
 90         KEY(IKEY) = ABS (RECORD(I1LOCU+1))
            GO TO 140
C                                       Descending ABS(v)
 100        KEY(IKEY) = ABS (RECORD(I1LOCV+1))
            GO TO 140
C                                       Ascending ABS(u)
 110        KEY(IKEY) = -ABS (RECORD(I1LOCU))
            GO TO 140
C                                       Ascending ABS(v)
 120        KEY(IKEY) = -ABS (RECORD(I1LOCV+1))
            GO TO 140
C                                       Unsorted
 130        KEY(IKEY) = 0.0
            GO TO 140
C                                       End of loop
 140     CONTINUE
      GO TO 999
C                                       Second stream
 150  DO 340 IKEY = 1,2
         GO TO (220, 230, 240, 250, 260, 270, 280, 290, 300,
     *      310, 320, 330), SCODE(IKEY)
C                                       Baseline number.
 220        KEY(IKEY) = -RECORD(I2LOCB+1)
            GO TO 340
C                                       Time.
 230        KEY(IKEY) = -RECORD(I2LOCT+1)
            GO TO 340
C                                       U
 240        KEY(IKEY) = -RECORD(I2LOCU+1)
            GO TO 340
C                                       V
 250        KEY(IKEY) = -RECORD(I2LOCV+1)
            GO TO 340
C                                       W
 260        KEY(IKEY) = -RECORD(I2LOCW+1)
            GO TO 340
C                                       Baseline length.
 270        XX = RECORD(I2LOCU+1)
            YY = RECORD(I2LOCV+1)
            KEY(IKEY) = -SQRT (XX*XX + YY*YY)
            GO TO 340
C                                       Baseline PA
 280        XX = RECORD(I2LOCU+1)
            YY = RECORD(I2LOCV+1)
            KEY(IKEY) = -ATAN2 (YY, XX+1.0E-20)
            GO TO 340
C                                       Descending ABS(u)
 290        KEY(IKEY) = ABS (RECORD(I2LOCU+1))
            GO TO 340
C                                       Descending ABS(v)
 300        KEY(IKEY) = ABS (RECORD(I2LOCV+1))
            GO TO 340
C                                       Ascending ABS(u)
 310        KEY(IKEY) = -ABS (RECORD(I2LOCU+1))
            GO TO 340
C                                       Ascending ABS(v)
 320        KEY(IKEY) = -ABS (RECORD(I2LOCV+1))
            GO TO 340
C                                       Unsorted
 330        KEY(IKEY) = 0.0
            GO TO 340
C                                       End of loop
 340     CONTINUE
 999  RETURN
      END
      SUBROUTINE HISANT
C-----------------------------------------------------------------------
C  HISANT copies history files and antenna files and adds history info.
C-----------------------------------------------------------------------
      INTEGER   NUMAPP
      PARAMETER (NUMAPP=13)
      CHARACTER NOTTYP(4)*2, CHTM8*8, HILINE*72, APTYPE(NUMAPP)*2
      INTEGER   LUN1, LUN2, LUN3, IVER, OVER, IER, ITEMP, IPTR, IERR,
     *   IMAXA, I, NANT, LIM1, LIM2, NONOT, SUBADD, NVER1, NVER2, NVER
      REAL      TIMADD
      DOUBLE PRECISION XDAT(100), XFREQ(100)
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DBCON.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2, LUN3 /27,28,29/
      DATA T, F /.TRUE.,.FALSE./
      DATA NONOT, NOTTYP /4,'AN','NX','SU','FQ'/
      DATA APTYPE /'CL','FG','TY','WX','IM','MC','PC','AT','CT','GC',
     *   'OB','SY','SN'/
C-----------------------------------------------------------------------
C                                       Write History.
C
      CALL HIINIT (2)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISK1, DISK3, FCNO(1), FCNO(3),
     *   CATBLK, BUFF2, BUFF1, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (7)
         GO TO 25
 10   WRITE (HILINE,2001) TSKNAM
      CALL HIADD (LUN2, HILINE, BUFF1, IERR)
      CALL HIOPEN (LUN1, DISK2, FCNO(2), BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 15
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         GO TO 20
 15   CALL HILOCT ('SRCH', LUN2, IPTR, IERR)
      ITEMP = HITAB(IPTR+2)
      CALL HICOPY (LUN1, BUFF2, LUN2, BUFF1, IERR)
      CALL HICLOS (LUN1, F, BUFF2, IER)
      IF (IERR.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (7)
         IF (IERR.LT.100) GO TO 25
         HITAB(IPTR+2) = ITEMP
C                                       New history
 20   CALL HENCO1 (TSKNAM, NAME1, CLASS1, SEQ1, DISK1, LUN2, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 25
      CALL HENCO2 (TSKNAM, NAME2, CLASS2, SEQ2, DISK2, LUN2, BUFF1,
     *   IERR)
      IF (IERR.NE.0) GO TO 25
      IMAXA = MAXA(2) + 1
      WRITE (HILINE,2002) TSKNAM, IMAXA
      CALL HIADD (LUN2, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 25
      CALL HENCOO (TSKNAM, NAME3, CLASS3, CATBLK(KIIMS), DISK3, LUN2,
     *   BUFF1, IERR)
 25   CONTINUE
C                                       Check no sub array option.
      IF (DOARR.LE.0.0) THEN
C                                       Get data on subarrays
         CALL ANTDAT (FCNO(3), DISK3, BUFF2, NANT, XDAT, XFREQ)
         LIM1 = MAXA(1)+1
         LIM2 = NANT
         DO 60 I = LIM1, LIM2
C                                        Write frequency and date in
C                                        history.
            WRITE (HILINE,2003) TSKNAM, I, XFREQ(I)
            CALL HIADD (LUN2, HILINE, BUFF1, IERR)
            CALL GREG (XDAT, CHTM8)
            WRITE (HILINE,2004) TSKNAM, I, CHTM8
            CALL HIADD (LUN2, HILINE, BUFF1, IERR)
 60         CONTINUE
         END IF
C                                       Reweighting factors
      WRITE (HILINE,2005) TSKNAM, REWT
      CALL HIADD (LUN2, HILINE, BUFF1, IERR)
      IF (IERR.NE.0) GO TO 25
C                                       Close history
      CALL HICLOS (LUN2, T, BUFF1, IERR)
C                                       Copy (non AN) tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN3, DISK1, DISK3, FCNO(1),
     *   FCNO(3), CATBLK, BUFF1(1,2), BUFF2, IERR)
C                                       Append table(s)
      DO 810 I = 1,NUMAPP
         CALL FNDEXT (APTYPE(I), CAT1, NVER1)
         CALL FNDEXT (APTYPE(I), CAT2, NVER2)
         NVER = MAX (NVER1, NVER2)
         IF (NVER.GT.1) THEN
            MSGTXT = 'WARNING: APPENDING MULTIPLE VERSIONS OF ' //
     *         APTYPE(I) // ' TABLES'
            CALL MSGWRT (7)
            MSGTXT = '         THIS MAY NOT BE DESIRABLE'
            CALL MSGWRT (7)
            END IF
         DO 800 IVER = 1,NVER
            OVER = IVER
C                                       Setup for second input file.
            TIMADD = (MAXA(1)+1) * 5.0
            SUBADD = (MAXA(1)+1)
            IF ((DOARR.GT.0) .AND. (IVER.LE.NVER1)) THEN
               TIMADD = DAYOFF(2)
               SUBADD = 0
               ITEMP = 0
C                                       Update times
               CALL DBCAPP (APTYPE(I), IVER, OVER, LUN1, LUN3, DISK1,
     *            DISK3, FCNO(1), FCNO(3), CATBLK, ITEMP, SOUTRA,
     *            FQ2TRA, DAYOFF(1), 0, T, IBUFF1, IBUFF2, IERR)
               END IF
            IF (IVER.LE.NVER2) CALL DBCAPP (APTYPE(I), IVER, OVER, LUN1,
     *         LUN3, DISK2, DISK3, FCNO(2), FCNO(3), CATBLK, NUMSOU,
     *         SOUTRA, FQ2TRA, TIMADD, SUBADD, F, IBUFF1, IBUFF2, IERR)
 800        CONTINUE
 810     CONTINUE
C                                       FQ tables
      CALL DBCOPF (IERR)
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISK3, FCNO(3), CATBLK, 'REST', BUFF2,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HISANT: ERROR',I3,' COPY/OPEN HISTORY FILE #1')
 1010 FORMAT ('HISANT: ERROR',I3,' COPY/OPEN HISTORY FILE #2')
 2001 FORMAT (A6,'/******Second input UV file history')
 2002 FORMAT (A6,'/ Second file has ',I3,' arrays')
 2003 FORMAT (A6,'/ Array ',I3,' Freq = ',1PD20.12,' Hz')
 2004 FORMAT (A6,'/ Array ',I3,' Ref. date =',A8)
 2005 FORMAT (A6,'REWEIGHT = ',1PE12.5, E12.5)
      END
      SUBROUTINE DBSOUR (IRET)
C-----------------------------------------------------------------------
C   DBSOUR merges the source tables of the two input files and returns
C   a translation table for the source ids of the second file.
C   Output:
C      IRET   I   Return error code  0 => ok, else error.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REST*4, VELTYP*8, VELDEF*8, SOUNAM*16, CALCOD*4,
     *   CHARS*27
      INTEGER   VER, LUN1, LUN2, SUKOL1(MAXSUC), SUKOL2(MAXSUC),
     *   SUNUM1(MAXSUC), SUNUM2(MAXSUC), NUMFST, JBUFF1(1024),
     *   JBUFF2(1024), NUMIF, IDSOU, QUAL, IDTEMP, NEXT, IERR, I, LUMIF,
     *   NRECIN, INREC, OUTREC, LOOP, SUFQD1, SUFQD2, II, JJ
      LOGICAL   T, F, NEW, MATCH, ISVELO
      DOUBLE PRECISION BANDW, RAEPO, DECEPO, EPOCH, RAAPP, DECAPP, PMRA,
     *   PMDEC, DFREQ, RAOBS, DECOBS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'DBCON.INC'
      LOGICAL WERR
      INTEGER   SUQUAL(MAXSOU)
      CHARACTER SUNAM(MAXSOU)*16, SUCODE(MAXSOU)*4
      REAL      FLUX(4,MAXIF)
      DOUBLE PRECISION LSRVEL(MAXIF), FREQO(MAXIF), RESTFQ(MAXIF)
      DOUBLE PRECISION DEPOCH(MAXSOU), DRAEPO(MAXSOU), DCEPO(MAXSOU),
     *   SEPS
C                                       If increasing MAXSOU may
C                                       have to change increase buffers:
      EQUIVALENCE (BUFF2, JBUFF1), (BUFF2(1025), JBUFF2)
      EQUIVALENCE (BUFF2(2049), SUQUAL)
      DATA REST /'REST'/
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
      CALL SOUINI ('READ', JBUFF1, DISK1, FCNO(1), VER, CAT1, LUN1,
     *   NUMIF, VELTYP, VELDEF, SUFQD1, INREC, SUKOL1, SUNUM1, IERR)
      MSGSUP = 0
C                                       If not there - quit
      IF (IERR.EQ.2) GO TO 999
      IF (IERR.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      LUMIF = NUMIF
      IF (NUMIF.NE.NNIF) THEN
         WRITE (MSGTXT,1000) '1st', NUMIF, NNIF
         CALL MSGWRT (8)
         END IF
C                                       Open second (output) file
      VER = 1
      IF (SUFQD1.EQ.-999) SUFQD1 = -1
      CALL SOUINI ('WRIT', JBUFF2, DISK3, FCNO(3), VER, CATBLK,
     *   LUN2, NUMIF, VELTYP, VELDEF, SUFQD1, OUTREC, SUKOL2,
     *   SUNUM2, IERR)
      IF (IERR.GT.0) THEN
         IRET = 3
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Get number of input records.
      NRECIN = JBUFF1(5)
      ISVELO = (VELDEF.EQ.'OPTICAL') .OR. (VELDEF.EQ.'RADIO')
C                                       Copy
      NUMFST = 0
      IRET = 6
      DO 200 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABSOU ('READ', JBUFF1, INREC, SUKOL1, SUNUM1, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1070) IERR, 'READ'
            GO TO 990
            END IF
C                                       Check number of sources
         IF (IDSOU.GT.MAXSOU) THEN
            IRET = 7
            WRITE (MSGTXT,1150) IDSOU, MAXSOU
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
C                                       Write
         CALL TABSOU ('WRIT', JBUFF2, OUTREC, SUKOL2, SUNUM2, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1070) IERR, 'WRIT'
            GO TO 990
            END IF
 200     CONTINUE
C                                       Close first input.
      CALL TABIO ('CLOS', 0, INREC, BUFF1, JBUFF1, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1070) IERR, 'CLOS'
         GO TO 990
         END IF
C                                       Open second input
      MSGSUP = 32000
      IRET = 0
      VER = 1
      CALL SOUINI ('READ', JBUFF1, DISK2, FCNO(2), VER, CAT2, LUN1,
     *   NUMIF, VELTYP, VELDEF, SUFQD2, INREC, SUKOL1, SUNUM1, IERR)
      MSGSUP = 0
C                                       If not there - quit
      IF (IERR.EQ.2) GO TO 800
      IF (IERR.GT.0) THEN
         IRET = 2
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      IF (NUMIF.NE.NNIF) THEN
         WRITE (MSGTXT,1000) '2nd', NUMIF, NNIF
         CALL MSGWRT (8)
         END IF
      IF (NUMIF.NE.LUMIF) THEN
         WRITE (MSGTXT,1001) LUMIF, NUMIF
         CALL MSGWRT (8)
         END IF
C                                       Get number of input records.
      NRECIN = JBUFF1(5)
      IRET = 6
      NUMSOU = NRECIN
      NEXT = NUMFST + 1
      MATCH = F
      CALL FILL (NUMSOU, NEXT, SOUTRA)
      ISVELO = ISVELO .OR. (VELDEF.EQ.'OPTICAL').OR.(VELDEF.EQ.'RADIO')
C                                       Copy, accumulating translation
C                                       table.
      DO 400 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABSOU ('READ', JBUFF1, INREC, SUKOL1, SUNUM1, IDSOU,
     *      SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IERR)
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1070) IERR, 'READ'
            GO TO 990
            END IF
C                                       Make translation table
         NEW = F
         DO 340 I = 1,NUMFST
            IDTEMP = I
            IF ((QUAL.EQ.SUQUAL(I)) .AND. (SOUNAM.EQ.SUNAM(I)) .AND.
     *         (CALCOD.EQ.SUCODE(I))) THEN
               IF ((ABS(DEPOCH(I)-EPOCH).LT.0.1) .AND.
     *            (ABS(DRAEPO(I)-RAEPO).LE.SEPS) .AND.
     *            (ABS(DCEPO(I)-DECEPO).LE.SEPS)) THEN
                  MATCH = T
                  GO TO 350
               ELSE
                  II = II + 1
                  IF (II.EQ.28) THEN
                     II = 1
                     JJ = JJ + 1
                     END IF
                  WRITE (MSGTXT,1080) SUNAM(I), SUQUAL(I)
                  CALL MSGWRT (6)
                  SOUNAM(15:15) = CHARS(JJ:JJ)
                  SOUNAM(16:16) = CHARS(II:II)
                  WRITE (MSGTXT,1081) SOUNAM
                  CALL MSGWRT (6)
                  END IF
               END IF
 340        CONTINUE
C                                       No match - new source.
         NEW = T
         IDTEMP = NEXT
         NEXT = NEXT + 1
C                                       Record source translation num
 350     SOUTRA(IDSOU) = IDTEMP
         IDSOU = IDTEMP
         NUMSOU = MAX (NUMSOU, IDSOU)
C                                       Check source coordinates match
         WERR = .FALSE.
         IF (.NOT. NEW) WERR = ((ABS(DEPOCH(IDSOU)-EPOCH).GT.0.1) .OR.
     *      (DABS(DRAEPO(IDSOU)-RAEPO).GT.SEPS) .OR.
     *      (DABS(DCEPO(IDSOU)-DECEPO).GT.SEPS))
         IF (WERR) THEN
            WRITE (MSGTXT,1080) SUNAM(IDSOU), SUQUAL(IDSOU)
            CALL MSGWRT (6)
            END IF
C                                       Check number of sources
         IF (IDSOU.GT.MAXSOU) THEN
            IRET = 7
            WRITE (MSGTXT,1150) IDSOU, MAXSOU
            GO TO 990
            END IF
C                                       Write
         IF (NEW) THEN
            DFREQ = FREQ1 - FREQ2
            DO 380 I = 1, NNIF
               FREQO(I) = FREQO(I) - DFREQ
  380          CONTINUE
            CALL TABSOU ('WRIT', JBUFF2, OUTREC, SUKOL2, SUNUM2, IDSOU,
     *         SOUNAM, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *         EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ,
     *         PMRA, PMDEC, IERR)
            END IF
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1070) IERR, 'WRIT'
            GO TO 990
            END IF
 400     CONTINUE
C                                       Close first input.
      CALL TABIO ('CLOS', 0, INREC, BUFF1, JBUFF1, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1070) IERR, 'CLOS'
         GO TO 990
         END IF
C                                       Close output table
 800  CALL TABIO ('CLOS', 0, OUTREC, BUFF2, JBUFF2, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1070) IERR, 'CLOS'
         GO TO 990
         END IF
      IRET = 0
C                                       Update header record
      CALL CATIO ('UPDT', DISK3, FCNO(3), CATBLK, REST, BUFF1, IERR)
      IF (IERR.NE.0) IRET = 5
C                                       rerun SETJY
      ISVELO = ISVELO .AND. ((DOUVM1) .OR. (DOUVM2))
      IF (ISVELO) THEN
         MSGTXT = 'YOU WILL NEED TO RUN SETJY TO RESET VELOCITIES'
         CALL MSGWRT (5)
         END IF
C                                       Warning message?
      IF (MATCH) THEN
         IF ((SUFQD1.EQ.-1) .AND. (SUFQD2.EQ.-999)) GO TO 999
         IF ((SUFQD1.EQ.-999) .AND. (SUFQD2.EQ.-1)) GO TO 999
         IF (SUFQD1.NE.SUFQD2) THEN
            IF (SUFQD1.EQ.-999) THEN
               WRITE (MSGTXT,1201) '1'
            ELSE IF (SUFQD1.EQ.-1) THEN
               WRITE (MSGTXT,1202) '1'
            ELSE
               WRITE (MSGTXT,1200) '1', SUFQD1
               END IF
            CALL MSGWRT (4)
            IF (SUFQD2.EQ.-999) THEN
               WRITE (MSGTXT,1201) '2'
            ELSE IF (SUFQD2.EQ.-1) THEN
               WRITE (MSGTXT,1202) '2'
            ELSE
               WRITE (MSGTXT,1200) SUFQD2
               END IF
            CALL MSGWRT (4)
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DBSOUR: ',A,' SU table NUMIF=',I3,' NOT',I3,' OF DATA')
 1001 FORMAT ('DBSOUR: TWO SU TABLES NUMIF =',2I3,' DIFFER!!!')
 1010 FORMAT ('DBSOUR: ERROR',I5,' OPENING OUTPUT TABLE')
 1020 FORMAT ('DBSOUR: ERROR',I5,' OPENING INPUT TABLE')
 1070 FORMAT ('DBSOUR: ERROR ',I3,2X,A4,'ING SU TABLE')
 1080 FORMAT ('DBSOUR: Coordinates disagree for ''',A,''' Qual',I5)
 1081 FORMAT ('DBSOUR: Source renamed ''',A,'''')
 1150 FORMAT ('DBSOUR: TOO MANY SOURCES:',I5, '>', I5,
     *   ' INCREASE TABLES')
 1200 FORMAT ('WARNING: SU table from file ',A,' has some values set ',
     *   'with FQID ',I4)
 1201 FORMAT ('         SU table from file ',A,' has no FQID keyword')
 1202 FORMAT ('         SU table from file ',A,' has not been modified')
      END
      SUBROUTINE DBCAPP (TYPE, INVER, OUTVER, LUN1, LUN2, VOL1, VOL2,
     *   CNO1, CNO2, CATBLK, NUMSOU, SOUTRA, FQ2TRA, TOFF, SUBADD,
     *   REWRIT, BUFF1, BUFF2, IRET)
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
C    FQTRA(*)   I     FQ translation list (2nd file only)
C    TOFF       R     Time offset in days
C      SUBADD   I      Subarray number out = in + SUBADD
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
     *   CATBLK(256), NUMSOU, SOUTRA(*), FQ2TRA(*), SUBADD, BUFF1(*),
     *   BUFF2(*), IRET
      REAL      TOFF
      LOGICAL   REWRIT
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REST*4, SIDKEY(7)*24
      INTEGER   NOTIME, IT2, IT, CATTMP(256)
      REAL      RECR(XBPRSZ)
      DOUBLE PRECISION RECD(XBPRSZ/2)
      INTEGER   NKEY, NREC, NCOL, DATP(128,2,2), IER, I, SOUKOL, FQKOL,
     *   RECORD(XBPRSZ), SUID, NRECIN, NRECOU, INREC, OUTREC, LOOP,
     *   TIMKOL, SUBKOL
      LOGICAL   T, F, NEW, GOTSOU, NEWSID, DOTIME, DOTIMR, DOTIMD,
     *   GOTFQ, NEWFQ, TABLE, EXIST, FITASC, OPEN1, OPEN2, GOTSUB
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
      CALL FNDCOL (1, SIDKEY(6), 10, T, BUFF1, SUBKOL, IER)
      IF (IER.NE.0) CALL FNDCOL (1, SIDKEY(7), 10, T, BUFF1, SUBKOL,
     *   IER)
      GOTSUB = (IER.EQ.0) .AND. (SUBADD.NE.0)
      CALL FNDCOL (1, SIDKEY(5), 10, T, BUFF1, FQKOL, IER)
      GOTFQ = IER.EQ.0
      CALL FNDCOL (1, SIDKEY(1), 10, T, BUFF1, SOUKOL, IER)
      IF (IER.NE.0) CALL FNDCOL (1, SIDKEY(2), 7, T, BUFF1, SOUKOL, IER)
      GOTSOU = IER.EQ.0
      IF (GOTSOU) SOUKOL = DATP(SOUKOL,1,1)
      IF (GOTFQ) FQKOL = DATP(FQKOL,1,1)
      IF (GOTSUB) SUBKOL = DATP(SUBKOL,1,1)
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
      NEWFQ = (NUMSOU.GT.0) .AND. GOTFQ
      IRET = 6
      DO 200 LOOP = 1,NRECIN
         INREC = LOOP
         CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
         IF (IER.GT.0) THEN
            WRITE (MSGTXT,1070) IER, 'READ', TYPE
            GO TO 990
C                                       Source translate
         ELSE IF (IER.EQ.0) THEN
            IF (NEWSID) THEN
               SUID = RECORD(SOUKOL)
               IF (SUID.GT.0) RECORD(SOUKOL) = SOUTRA(SUID)
               END IF
C                                       FQ translate
            IF (NEWFQ) THEN
               SUID = RECORD(FQKOL)
               IF (SUID.GT.0) RECORD(FQKOL) = FQ2TRA(SUID)
               END IF
C                                       FQ translate
            IF (GOTSUB) THEN
               SUID = RECORD(SUBKOL)
               IF (SUID.GT.0) RECORD(SUBKOL) = SUID + SUBADD
               END IF
C                                       Modify time?
            IF (DOTIMR) THEN
               DO 150 IT = 1,NOTIME
                  RECR(TIMKOL+IT-1) = RECR(TIMKOL+IT-1) + TOFF
 150              CONTINUE
               END IF
            IF (DOTIMD) THEN
               DO 160 IT = 1,NOTIME
                  RECD(TIMKOL+IT-1) = RECD(TIMKOL+IT-1) + TOFF
 160              CONTINUE
               END IF
            CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
            IF (IER.GT.0) THEN
               WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
               GO TO 990
               END IF
            OUTREC = OUTREC + 1
            END IF
 200     CONTINUE
C                                       Close tables.
      CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (IER.GT.0) THEN
         WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
         GO TO 990
         END IF
      OPEN1 = F
      BUFF2(5) = OUTREC - 1
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
      SUBROUTINE DBANT (IRET)
C-----------------------------------------------------------------------
C   DBANT will write separate AN files for each subarray if DOARR < 1.
C   If the subarrays are to be merged DBANT will try to consolidate the
C   AN tables from both input files in writing each output AN table.
C
C   In the latter case the following restrictions apply.
C   i) No antenna renumbering is performed as yet.  The antenna numbers
C      are expected to be consistent. The output AN file will be the
C      union of the two input AN files.
C   ii) AN files will not be consolidated if the AN headers differ in
C      important ways.
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
      INTEGER   ANKOLS(MAXANC,3), ANNUMV(MAXANC,3), NUMORB(2),
     *   NOPCAL(2), ANTNIF(2), ANFQID(2), IANRNO(3)
      CHARACTER ANAME(2)*8, RDATE(2)*8, TIMSYS(2)*8, XYZHAN(2)*8,
     *   TFRAME(2)*8
      REAL      POLRXY(2,2), UT1UTC(2), DATUTC(2)
      DOUBLE PRECISION  ARRAYC(3,2), GSTIA0(2), DEGPDY(2), SAFREQ(2)
C                                       Declarations for TABAN
      INTEGER   NOSTA(2), MNTSTA(2)
      CHARACTER ANNAME(2)*8, POLTYA(2)*2, POLTYB(2)*2
      REAL      STAXOF(2), DIAMAN(2), FWHMAN(MAXIF,2), POLAA(2),
     *   POLCA(2*MAXIF,2), POLAB(2), POLCB(2*MAXIF,2)
      DOUBLE PRECISION  STAXYZ(3,2), ORBPRM(6,2)
C                                       End modified include.
      INCLUDE 'DBCON.INC'
      LOGICAL WEXIST(2), WMERGE, WERR, WERR1, WERR2, WERR3,
     *   WMATCH, WOK(2)
      CHARACTER LANAME(MAXANT)*8, LBLNK8*8
      DOUBLE PRECISION DIFFJD(2), DJREF, DJUL(2), DBVEC, CEPS
      DOUBLE PRECISION FSCALE
      INTEGER IEXTAN, ILUN(3), IOUT, J, K, M, CATBKA(256,2), JLIM,
     *   IDISKN(2), IANTMX, IANREC(MAXANT), IANTRA(MAXANT), IVER,
     *   OVER, LIM, NEXTNT, NSTNS(2), IERR, I, IAUSED(MAXANT)
      EQUIVALENCE (CAT1, CATBKA)
      DATA ILUN /27, 28, 29/
      DATA LBLNK8 /'        '/
C----------------------------------------------------------------------
C                                       Initialisation.
      IRET = 0
      IDISKN(1) = DISK1
      IDISKN(2) = DISK2
      CALL FILL (MAXANT, 0, IAUSED)
C                                       eps for differences in
C                                       antenna coords, set = 1cm
      CEPS = 1.0E-2
C                                       Find no. subarrays in each file
      CALL EXTNO ('AN', MAXA)
      NEXTNT = MAX (MAXA(1), MAXA(2))
C                                       Branch if AN files are to be
C                                       written for separate subarrays.
      IF (DOARR.LE.0) GO TO 510
C                                       Else try to consolidate
C                                       the AN files for each vers no.
C                                       Ref date in catalog header
      CALL JULDAY (NEWRD, DJREF)
C                                       Loop over the AN table extents
      DO 500 IEXTAN = 1,NEXTNT
C                                       Do tables exist for both files ?
         DO 100 J = 1,2
            NSTNS(J) = 0
            WEXIST(J) = .FALSE.
            DIFFJD(J) = DBLANK
            IF (IEXTAN.LE.(MAXA(J))) THEN
               CALL ANTINI ('READ', BUFF1(1,J), IDISKN(J), FCNO(J),
     *            IEXTAN, CATBKA(1,J), ILUN(J), IANRNO(J), ANKOLS(1,J),
     *            ANNUMV(1,J), ARRAYC(1,J), GSTIA0(J), DEGPDY(J),
     *            SAFREQ(J), RDATE(J), POLRXY(1,J), UT1UTC(J),
     *            DATUTC(J), TIMSYS(J), ANAME(J), XYZHAN(J), TFRAME(J),
     *            NUMORB(J), NOPCAL(J), ANTNIF(J), ANFQID(J), IERR)
               WEXIST(J) = IERR.EQ.0
               IF (IERR.EQ.0) THEN
                  CALL JULDAY (RDATE(J), DJUL(J))
                  DIFFJD(J) = ABS (DJUL(J) - DJREF)
                  NSTNS(J) = IBUFF1(5,J)
                  END IF
               END IF
 100        CONTINUE
C                                       Can the tables be merged ?
C                                       This is not possible if
C                                       either AN file does
C                                       not exist or the AN
C                                       headers disagree in important
C                                       ways. NO antenna renumbering
C                                       is performed.
         WMERGE = WEXIST(1) .AND. WEXIST(2)
         DO 120 M = 1,3
            WMERGE = WMERGE .AND.
     *         (ABS (ARRAYC(M,1)-ARRAYC(M,2)).LT.CEPS)
 120        CONTINUE
         IF ((NOPCAL(1).GT.0).OR.(NOPCAL(2).GT.0)) WMERGE = WMERGE
     *      .AND. (NOPCAL(1).EQ.NOPCAL(2))
         IF ((NUMORB(1).GT.0).OR.(NUMORB(2).GT.0)) WMERGE = WMERGE
     *      .AND. (NUMORB(1).EQ.NUMORB(2))
         IF ((ANFQID(1).GT.0).OR.(ANFQID(2).GT.0)) WMERGE = WMERGE
     *      .AND. (ANFQID(1).EQ.ANFQID(2))
C                                       Merge
         IF (WMERGE) THEN
C                                       Compare AN hdrs if same ref date
            IF (DIFFJD(1).EQ.DIFFJD(2)) THEN
               WERR = (ABS (GSTIA0(1)-GSTIA0(2)).GT.1.0D-6) .OR.
     *                (ABS (DEGPDY(1)-DEGPDY(2)).GT.1.0D-6) .OR.
     *                (SAFREQ(1).NE.SAFREQ(2)) .OR.
     *                (POLRXY(1,1).NE.POLRXY(1,2)) .OR.
     *                (POLRXY(2,1).NE.POLRXY(2,2)) .OR.
     *                (UT1UTC(1).NE.UT1UTC(2)) .OR.
     *                (DATUTC(1).NE.DATUTC(2)) .OR.
     *                (TIMSYS(1).NE.TIMSYS(2))
               IF (WERR) THEN
                  WRITE (MSGTXT,1000) IEXTAN
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1001)
                  CALL MSGWRT (6)
                  END IF
               END IF
C                                       Select the output hdr.
            IOUT = 1
            IF (DIFFJD(2).LT.DIFFJD(1)) IOUT = 2
C                                       Create the output AN table.
            DO 140 K = 1,MAXANC
               ANNUMV(K,3) = ANNUMV(K,IOUT)
 140           CONTINUE
C                                       Scale frequency keyword if
C                                       needed
            FSCALE = 1.0D0
            IF (DOUVM1) FSCALE = UVMUL1
            SAFREQ(IOUT) = SAFREQ(IOUT) * FSCALE
            CALL ANTINI ('WRIT', BUFF2, DISK3, FCNO(3), IEXTAN, CATBLK,
     *         ILUN(3), IANRNO(3), ANKOLS(1,3), ANNUMV(1,3),
     *         ARRAYC(1,IOUT), GSTIA0(IOUT), DEGPDY(IOUT),
     *         SAFREQ(IOUT), RDATE(IOUT), POLRXY(1,IOUT),
     *         UT1UTC(IOUT), DATUTC(IOUT), TIMSYS(IOUT), ANAME(IOUT),
     *         XYZHAN(IOUT), TFRAME(IOUT), NUMORB(IOUT), NOPCAL(IOUT),
     *         ANTNIF(IOUT), ANFQID(IOUT), IERR)
            IF (IERR.NE.0) GO TO 999
C                                       Initialise the AN#2 index
            DO 150 K = 1,MXANT
               LANAME(K) = LBLNK8
               IANREC(K) = 0
               IANTRA(K) = 0
 150           CONTINUE
C                                       Compile the AN#2 index.
            DO 180 K = 1,NSTNS(2)
               CALL TABAN ('READ', BUFF1(1,2), IANRNO(2), ANKOLS(1,2),
     *            ANNUMV(1,2), ANNAME(2), STAXYZ(1,2), ORBPRM(1,2),
     *            NOSTA(2), MNTSTA(2), STAXOF(2), DIAMAN(2),
     *            FWHMAN(1,2), POLTYA(2), POLAA(2), POLCA(1,2),
     *            POLTYB(2), POLAB(2), POLCB(1,2), IERR)
               IF (IERR.GT.0) GO TO 999
C                                       Add to index.
               IF (NOSTA(2).LE.MXANT) THEN
                  LANAME(NOSTA(2)) = ANNAME(2)
                  IANREC(NOSTA(2)) = IANRNO(2) - 1
                  IANTMX = MAX (IANREC(NOSTA(2)), IANTMX)
               ELSE
                  WRITE (MSGTXT,1050)
                  CALL MSGWRT (8)
                  IRET = 1
                  GO TO 999
                  END IF
 180           CONTINUE
C                                       Write merged output file.
C                                       Read through AN #1.
            DO 290 K = 1,NSTNS(1)
               CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV,
     *            ANNAME, STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN,
     *            FWHMAN,  POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB,
     *            IERR)
               IF (IERR.NE.0) GO TO 999
C                                       Search the AN#2 index.
               I = 1
 210           IF ((I.GT.MXANT) .OR. (LANAME(I).EQ.ANNAME(1)))
     *            GO TO 220
                  I = I + 1
                  GO TO 210
C                                       Read record from AN#2 if found.
 220           JLIM = 1
               WOK(2) = .FALSE.
               IF (I.LE.MXANT) THEN
                  IANRNO(2) = IANREC(I)
                  CALL TABAN ('READ', BUFF1(1,2), IANRNO(2),
     *               ANKOLS(1,2), ANNUMV(1,2), ANNAME(2), STAXYZ(1,2),
     *               ORBPRM(1,2), NOSTA(2), MNTSTA(2), STAXOF(2),
     *               DIAMAN(2), FWHMAN(1,2), POLTYA(2), POLAA(2),
     *               POLCA(1,2), POLTYB(2), POLAB(2), POLCB(1,2), IERR)
                  IF (IERR.GT.0) GO TO 999
                  IANTRA(I) = NOSTA(1)
                  IANREC(I) = 0
                  JLIM = 2
                  END IF
C                                       Are the antenna coords valid ?
               DO 240 J = 1,JLIM
                  WOK(J) = .TRUE.
                  DO 230 M = 1,3
                     WOK(J) = WOK(J) .AND. (STAXYZ(M,J).NE.DBLANK)
 230                 CONTINUE
                  IF (WOK(J)) THEN
                     DBVEC = SQRT (STAXYZ(1,J) * STAXYZ(1,J) +
     *                  STAXYZ(2,J) * STAXYZ(2,J) +
     *                  STAXYZ(3,J) * STAXYZ(3,J))
                     WOK(J) = DBVEC.GT.2
                     END IF
 240              CONTINUE
C                                       Check AN entries for consistency
               IF (WOK(1) .AND. WOK(2)) THEN
                  WERR = (DABS(STAXYZ(1,1)-STAXYZ(1,2)).GT.CEPS) .OR.
     *                   (DABS(STAXYZ(2,1)-STAXYZ(2,2)).GT.CEPS) .OR.
     *                   (DABS(STAXYZ(3,1)-STAXYZ(3,2)).GT.CEPS) .OR.
     *                   (MNTSTA(1).NE.MNTSTA(2)) .OR.
     *                   (STAXOF(1).NE.STAXOF(2))
                  WERR1 = (POLTYA(1).NE.POLTYA(2)) .OR.
     *                    (POLAA(1).NE.POLAA(2)) .OR.
     *                    (POLTYB(1).NE.POLTYB(2)) .OR.
     *                    (POLAB(1).NE.POLAB(2))
                  WERR2 = .FALSE.
                  DO 260 M = 1,NUMORB(1)
                     WERR2 = WERR2 .OR. (ORBPRM(M,1).NE.ORBPRM(M,2))
 260                 CONTINUE
                  WERR3 = .FALSE.
                  DO 280 M = 1,NOPCAL(1)
                     WERR3 = WERR3 .OR. (POLCA(M,1).NE.POLCA(M,2))
     *                  .OR. (POLCB(M,1).NE.POLCB(M,2))
 280                 CONTINUE
                  WERR = WERR .OR. WERR1 .OR. WERR2 .OR. WERR3
                  IF (WERR) THEN
                     WRITE (MSGTXT,1010) ANNAME(1), IEXTAN
                     CALL MSGWRT (6)
                     WRITE (MSGTXT,1001)
                     CALL MSGWRT (6)
                     END IF
                  END IF
C                                       Select output record and
C                                       write to output table.
               IOUT = 1
               IF (WOK(2) .AND. (.NOT. WOK(1))) IOUT = 2
               IAUSED(NOSTA(IOUT)) = 1
               CALL TABAN ('WRIT', BUFF2, IANRNO(3), ANKOLS(1,3),
     *            ANNUMV(1,3), ANNAME(IOUT), STAXYZ(1,IOUT),
     *            ORBPRM(1,IOUT), NOSTA(IOUT), MNTSTA(IOUT),
     *            STAXOF(IOUT), DIAMAN(IOUT), FWHMAN(1,IOUT),
     *            POLTYA(IOUT), POLAA(IOUT), POLCA(1,IOUT),
     *            POLTYB(IOUT), POLAB(IOUT), POLCB(1,IOUT), IERR)
               IF (IERR.GT.0) GO TO 999
C                                       Next AN extent
 290           CONTINUE
C                                       are there more antennas to get
C                                       from 2
            DO 310 I = 1,MXANT
               IF (IANREC(I).GT.0) THEN
                  IANRNO(2) = IANREC(I)
                  CALL TABAN ('READ', BUFF1(1,2), IANRNO(2),
     *               ANKOLS(1,2), ANNUMV(1,2), ANNAME(2), STAXYZ(1,2),
     *               ORBPRM(1,2), NOSTA(2), MNTSTA(2), STAXOF(2),
     *               DIAMAN(2), FWHMAN(1,2), POLTYA(2), POLAA(2),
     *               POLCA(1,2), POLTYB(2), POLAB(2), POLCB(1,2), IERR)
                  IF (IERR.GT.0) GO TO 999
                  IF (IAUSED(NOSTA(2)).GT.0) THEN
                     MSGTXT = 'ANTENNA NUMBER FROM 2ND ALREADY USED'
                     CALL MSGWRT (7)
                     WRITE (MSGTXT,1001)
                     CALL MSGWRT (6)
                  ELSE
                     IOUT = 2
                     IAUSED(NOSTA(IOUT)) = 1
                     IANTRA(I) = NOSTA(2)
                     CALL TABAN ('WRIT', BUFF2, IANRNO(3), ANKOLS(1,3),
     *                  ANNUMV(1,3), ANNAME(IOUT), STAXYZ(1,IOUT),
     *                  ORBPRM(1,IOUT), NOSTA(IOUT), MNTSTA(IOUT),
     *                  STAXOF(IOUT), DIAMAN(IOUT), FWHMAN(1,IOUT),
     *                  POLTYA(IOUT), POLAA(IOUT), POLCA(1,IOUT),
     *                  POLTYB(IOUT), POLAB(IOUT), POLCB(1,IOUT), IERR)
                     IF (IERR.GT.0) GO TO 999
                     END IF
                 END IF
 310          CONTINUE
C                                       Are the ant nos consistent ?
            WMATCH = .TRUE.
            DO 320 K = 1,IANTMX
               IF ((LANAME(K).NE.LBLNK8).AND.(IANTRA(K).EQ.0))
     *            WMATCH = .FALSE.
               IF ((IANTRA(K).NE.0).AND.(IANTRA(K).NE.K))
     *            WMATCH = .FALSE.
 320           CONTINUE
            IF (.NOT.WMATCH) THEN
               WRITE (MSGTXT,1020) IEXTAN
               CALL MSGWRT (6)
               WRITE (MSGTXT,1001)
               CALL MSGWRT (6)
               END IF
C                                       Close output AN table.
            MSGSUP = 32000
            CALL TABIO ('CLOS', 0, IANRNO(3), BUFF2, BUFF2, IERR)
            MSGSUP = 0
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1040) IERR, IEXTAN
               CALL MSGWRT (7)
               END IF
C                                       Print message
            WRITE (MSGTXT,1070) IDISKN(1), FCNO(1), IEXTAN,
     *         IDISKN(2), FCNO(2), IEXTAN
            CALL MSGWRT (6)
C                                       End if (MERGE)
            END IF
C                                       Close input AN tables
         DO 400 J = 1,2
            IF (WEXIST(J)) THEN
               MSGSUP = 32000
               CALL TABIO ('CLOS', 0, IANRNO(J), BUFF1(1,J),
     *            BUFF1(1,J), IERR)
               MSGSUP = 0
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1040) IERR, IEXTAN
                  CALL MSGWRT (7)
                  END IF
               END IF
 400        CONTINUE
C                                       If tables not merged then
C                                       copy a table directly from
C                                       either of the input files
C                                       preferably file #1.

         IF (.NOT. WMERGE) THEN
            J = 0
            IF (WEXIST(1)) J = 1
            IF (WEXIST(2) .AND. (.NOT. WEXIST(1))) J = 2
            IVER = IEXTAN
            OVER = 0
            IF (J.NE.0) THEN
               CALL TABCOP ('AN', IVER, OVER, ILUN(J), ILUN(3),
     *            IDISKN(J), DISK3, FCNO(J), FCNO(3), CATBLK,
     *            BUFF1(1,2), BUFF2, IERR)
               IF (IERR.GT.0) THEN
                  WRITE (MSGTXT,1030) IERR, IEXTAN
                  CALL MSGWRT (7)
                  END IF
               END IF
            END IF
C                                       Next AN extent
 500  CONTINUE
      GO TO 999
C                                       Write separate AN files for
C                                       each subarray.
 510  CONTINUE
C                                       Copy antenna (AN) files from
C                                       input file #1.
         LIM = MAXA(1)
         DO 505 I = 1,LIM
            IVER = I
            OVER = 0
            CALL TABCOP ('AN', IVER, OVER, ILUN(1), ILUN(3), DISK1,
     *         DISK3, FCNO(1), FCNO(3), CATBLK, BUFF1(1,2),
     *         BUFF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, IVER
               CALL MSGWRT (7)
               END IF
 505        CONTINUE
C                                       Copy AN files from inp file#2.
         LIM = MAXA(2)
         IVER = 1
         OVER = MAXA(1) + 1
         DO 550 I = 1,LIM
            CALL TABCOP ('AN', IVER, OVER, ILUN(2), ILUN(3), DISK2,
     *         DISK3, FCNO(2), FCNO(3), CATBLK, BUFF1(1,2), BUFF2, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1030) IERR, IVER
               CALL MSGWRT (7)
               END IF
            IVER = IVER + 1
            OVER = OVER + 1
 550        CONTINUE
C                                       Exit
 999  RETURN
C---------------------------------------------------------------------
 1000 FORMAT ('DBANT: AN headers may be incompatible for version ',I4)
 1001 FORMAT ('CONTINUING, BUT YOU SHOULD RUN MATCH AND THEN DBCON')
 1010 FORMAT ('DBANT: Data for ',A8,' inconsistent in AN vers',I4)
 1020 FORMAT ('DBANT: Antenna nos. incompatible for AN vers',I4)
 1030 FORMAT ('DBANT: ERROR',I3,' COPYING AN FILE',I4)
 1040 FORMAT ('DBANT: ERROR',I3,' CLOSING AN FILE',I4)
 1050 FORMAT ('DBANT: Parameter MXANT needs to be increased')
 1070 FORMAT ('Consolidate AN files from vol/cno/vers',I3,I4,I3,
     *   ' and',I3,I4,I3)
      END
      SUBROUTINE DBIF (IRET)
C-----------------------------------------------------------------------
C   Check that any FQ tables are consistent and arrange any renumbering
C   Output parameter:
C      IRET       I      <>0 => error.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBCON.INC'
C
      LOGICAL   WERR, WEXIST(2), RESET, WARN
      DOUBLE PRECISION IFFREQ(MAXIF,MAXFQ,2)
      REAL      IFCHW(MAXIF,MAXFQ,2), IFTBW(MAXIF,MAXFQ,2), CHWTOL
      INTEGER   NUMIF(2), JBUFF1(512,2), CATBKA(256,2), IDISKN(2),
     *   ILUN(2), J, K, IFQRNO(2), FQKOLS(MAXFQC,2), FQNUMV(MAXFQC,2),
     *   IERR, NFQID(2), IVER, M, IFSIDE(MAXIF,MAXFQ,2), I,
     *   FQID(MAXFQ,2), NFQTAB(2), MAXFQN(2)
      CHARACTER BNDCOD(MAXIF,MAXFQ,2)*8
      EQUIVALENCE (CAT1, CATBKA)
      DATA ILUN /27, 28/
C-----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      IDISKN(1) = DISK1
      IDISKN(2) = DISK2
      RESET = (XFQTOL.GT.-0.5) .AND. (I1LOCQ.GE.0) .AND. (I2LOCQ.GE.0)
      WARN = .FALSE.
      XFQTOL = MAX (1.0, XFQTOL) * 1.E3
      CHWTOL = XFQTOL / MAX (1, CAT1(KINAX+J1LOCF))
C                                       Are FQ tables present ?
      CALL EXTNO ('FQ', NFQTAB)
C                                       Determine no of IF's in
C                                       each FQ table.
      DO 10 J = 1,2
         NFQID(J) = 0
         NUMIF(J) = 0
         WEXIST(J) = .FALSE.
         IVER = 1
         IF (NFQTAB(J).GT.0) THEN
            CALL FQINI ('READ', JBUFF1(1,J), IDISKN(J), FCNO(J), IVER,
     *         CATBKA(1,J), ILUN(J), IFQRNO(J), FQKOLS(1,J),
     *         FQNUMV(1,J), NUMIF(J), IERR)
            IF (IERR.EQ.0) THEN
               NFQID(J) = JBUFF1(5,J)
               WEXIST(J) = .TRUE.
               END IF
            END IF
 10      CONTINUE
C                                       Skip if no FQ tables.
      IF ((NUMIF(1).EQ.0) .AND. (NUMIF(2).EQ.0)) GO TO 900
      IF (NUMIF(1).NE.NUMIF(2)) THEN
         MSGTXT = 'NUMBER IFS DO NOT MATCH IN FQ TABLES: QUIT'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 900
         END IF
C                                       Check FQ table entries.
      DO 20 J = 1,2
         MAXFQN(J) = 0
         DO 15 K = 1,NFQID(J)
            CALL TABFQ ('READ', JBUFF1(1,J), IFQRNO(J), FQKOLS(1,J),
     *         FQNUMV(1,J), NUMIF(J), FQID(K,J), IFFREQ(1,K,J),
     *         IFCHW(1,K,J), IFTBW(1,K,J), IFSIDE(1,K,J), BNDCOD(1,K,J),
     *         IRET)
            IF (IRET.NE.0) GO TO 900
            MAXFQN(J) = MAX (MAXFQN(J), FQID(K,J))
 15         CONTINUE
 20      CONTINUE
C                                       Compare the FQ tables.
      J = MAXFQN(1)
      DO 50 I = 1,NFQID(2)
         DO 40 K = 1,NFQID(1)
C                                       look for exact match
            WERR = .FALSE.
            DO 25 M = 1,NUMIF(1)
               WERR = WERR .OR.
     *            (ABS(IFFREQ(M,K,1)-IFFREQ(M,I,2)).GT.10.D0) .OR.
     *            (ABS(IFCHW(M,K,1)-IFCHW(M,I,2)).GT.10.D0) .OR.
     *            (IFSIDE(M,K,1).NE.IFSIDE(M,I,2)) .OR.
     *            (BNDCOD(M,K,1).NE.BNDCOD(M,I,2))
 25            CONTINUE
            IF (.NOT.WERR) THEN
               IF ((I1LOCQ.GE.0) .AND. (I2LOCQ.GE.0)) THEN
                  FQ2TRA(FQID(I,2)) = FQID(K,1)
                  GO TO 50
               ELSE IF (XFQTOL.LE.-0.5) THEN
                  FQ2TRA(FQID(I,2)) = FQID(I,2)
                  WARN = WARN .OR. (FQID(I,2).NE.FQID(K,1))
                  GO TO 50
                  END IF
               END IF
C                                       look for inexact match
            WERR = .FALSE.
            DO 30 M = 1,NUMIF(1)
               WERR = WERR .OR.
     *            (ABS(IFFREQ(M,K,1)-IFFREQ(M,I,2)).GT.XFQTOL) .OR.
     *            (ABS(IFCHW(M,K,1)-IFCHW(M,I,2)).GT.XFQTOL) .OR.
     *            (IFSIDE(M,K,1).NE.IFSIDE(M,I,2)) .OR.
     *            (BNDCOD(M,K,1).NE.BNDCOD(M,I,2))
 30            CONTINUE
            IF (.NOT.WERR) THEN
               IF (RESET) THEN
                  FQ2TRA(FQID(I,2)) = FQID(K,1)
               ELSE
                  FQ2TRA(FQID(I,2)) = FQID(I,2)
                  WARN = WARN .OR. (FQID(I,2).NE.FQID(K,1))
                  END IF
               GO TO 50
               END IF
 40         CONTINUE
         J = J + 1
         IF (RESET) THEN
            FQ2TRA(FQID(I,2)) = J
         ELSE
            FQ2TRA(FQID(I,2)) = FQID(I,2)
            WARN = WARN .OR. (FQID(I,2).NE.J)
            END IF
 50      CONTINUE
C                                       Close tables.
 900  IF (WARN) THEN
         MSGTXT = 'FQIDS HAVE NOT BEEN RESET AS THEY SHOULD DUE' //
     *      ' TO FQTOL'
         CALL MSGWRT (7)
         MSGTXT = 'OR DUE TO THERE BEING NO FQ RANDOM PARAMETER'
         IF (XFQTOL.GT.-0.5) CALL MSGWRT (7)
         END IF
      DO 910 J = 1,2
         MSGSUP = 32000
         IF (WEXIST(J)) CALL TABIO ('CLOS', 0, IFQRNO(J), JBUFF1(1,J),
     *      JBUFF1(1,J), IERR)
         MSGSUP = 0
 910     CONTINUE
C                                       Exit
 999  RETURN
      END
      SUBROUTINE DBCOPF (IRET)
C--------------------------------------------------------------------
C   Write FQ tables info to output
C   Output parameter:
C      IRET       I      <>0 => error.
C--------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'DBCON.INC'
C
      LOGICAL  WOUT
      DOUBLE PRECISION IFFREQ(MAXIF)
      REAL      IFCHW(MAXIF), IFTBW(MAXIF)
      INTEGER   NUMIF, JBUFF1(512), CATBKA(256,2), ILUN(2), J,
     *   K, IFQRNO(2), FQKOLS(MAXFQC,2), FQNUMV(MAXFQC,2), IERR, NFQID,
     *   IVER, IFSIDE(MAXIF), I, NW, FQID, NFQTAB(2), FQWRI(MAXFQ),
     *   JBUFF2(512), IAXFRQ, FQIDS(MAXFQ)
      CHARACTER BNDCOD(MAXIF)*8
      EQUIVALENCE (CAT1, CATBKA)
      DATA ILUN /27, 28/
C-----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
      CALL FILL (MAXFQ, 0, FQWRI)
C                                       Are FQ tables present ?
      CALL EXTNO ('FQ', NFQTAB)
      WOUT = .FALSE.
      CALL AXEFND (4, 'FREQ', CATBLK(KIDIM), CATBLK(KHCTP), IAXFRQ,
     *   IERR)
C                                       Determine no of IF's in
C                                       each FQ table.
      NW = 0
      DO 100 J = 1,2
         IF (NFQTAB(J).GT.0) THEN
            IVER = 1
            CALL FQINI ('READ', JBUFF1, FVOL(J), FCNO(J), IVER,
     *         CATBKA(1,J), ILUN(1), IFQRNO(1), FQKOLS(1,1),
     *         FQNUMV(1,1), NUMIF, IRET)
            IF (IRET.NE.0) GO TO 999
            NFQID = JBUFF1(5)
            IF (.NOT.WOUT) THEN
               CALL FQINI ('WRIT', JBUFF2, FVOL(3), FCNO(3), IVER,
     *            CATBLK, ILUN(2), IFQRNO(2), FQKOLS(1,2), FQNUMV(1,2),
     *            NUMIF, IRET)
               IF (IRET.NE.0) GO TO 999
               WOUT = .TRUE.
               END IF
            DO 25 K = 1,NFQID
               CALL TABFQ ('READ', JBUFF1, IFQRNO(1), FQKOLS(1,1),
     *            FQNUMV(1,1), NUMIF, FQID, IFFREQ, IFCHW, IFTBW,
     *            IFSIDE, BNDCOD, IRET)
               IF (IRET.NE.0) GO TO 900
               IF (J.EQ.2) THEN
                  FQID = FQ2TRA(FQID)
                  DO 10 I = 1,NW
                     IF (FQID.EQ.FQIDS(I)) GO TO 25
 10                  CONTINUE
                  END IF
               NW = NW + 1
               FQIDS(NW) = FQID
               IF (IAXFRQ.GT.0) THEN
                  DO 15 I = 1,NUMIF
                     IFTBW(I) = ABS (IFCHW(I) * CATBLK(KINAX+IAXFRQ))
                     IFFREQ(I) = IFFREQ(I) - DELTF(J) +
     *                  DELTC(J) * IFCHW(I)
 15                  CONTINUE
                  END IF
               CALL TABFQ ('WRIT', JBUFF2, IFQRNO(2), FQKOLS(1,2),
     *            FQNUMV(1,2), NUMIF, FQID, IFFREQ, IFCHW, IFTBW,
     *            IFSIDE, BNDCOD, IRET)
               IF (IRET.NE.0) GO TO 900
 25            CONTINUE
            CALL TABIO ('CLOS', 0, IFQRNO(1), JBUFF1, JBUFF1, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 100     CONTINUE
C
 900  IF (WOUT) CALL TABIO ('CLOS', 0, IFQRNO(2), JBUFF2, JBUFF2, IERR)
C                                       Exit
 999  RETURN
      END
      SUBROUTINE EXTNO (LTYPE, MTABL)
C--------------------------------------------------------------------
C   Determine the maximum version number of a given table type (eg
C   'AN','FQ',etc) for each input file. This information is obtained
C   from the catalog header.
C   Input:
C      LTYPE  C*2    Table type.
C   Ouput:
C      MTABL  I(2)   Max version no found
C   Input via common:
C      CAT1   I(*)   Catalog header for file 1.
C      CAT2   I(*)   Catalog header for file 2.
C--------------------------------------------------------------------
      CHARACTER LTYPE*2
      INTEGER   MTABL(2)
C
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'DBCON.INC'
      INTEGER   J, CATBKA(256,2)
      EQUIVALENCE (CAT1, CATBKA)
C--------------------------------------------------------------------
      DO 30 J = 1,2
         CALL FNDEXT (LTYPE, CATBKA(1,J), MTABL(J))
 30      CONTINUE
C
 999  RETURN
      END

LOCAL INCLUDE 'BDAPL.INC'
C                                       local include for BDAPL
      INTEGER   SEQ1, SEQ2, SEQO, DISK1, DISK2, DISKO, CNO1, CNO2, CNOO,
     *   VERS, NUMPOL, NUMFRQ, NUMIF, NUMANT, CATOLD(256,2)
      CHARACTER NAME1*12, NAME2*12, NAMEO*12, CLAS1*6, CLAS2*6, CLASO*6
      HOLLERITH XNAM1(3), XNAM2(3), XNAMO(3), XCLAS1(2), XCLAS2(2),
     *   XCLASO(2)
      REAL      XSEQ1, XSEQ2, XSEQO, XDISK1, XDISK2, XDISKO, XVERS,
     *   XCENT, DIFPIX
      DOUBLE PRECISION UVSCAL
      COMMON /INPARM/ XNAM1, XCLAS1, XSEQ1, XDISK1, XNAM2, XCLAS2,
     *   XSEQ2, XDISK2, XVERS, XNAMO, XCLASO, XSEQO, XDISKO, XCENT
      COMMON /VALS/ CATOLD, UVSCAL, SEQ1, SEQ2, SEQO, DISK1, DISK2,
     *   DISKO, CNO1, CNO2, CNOO, VERS, NUMPOL, NUMFRQ, NUMIF, NUMANT,
     *   DIFPIX
      COMMON /CHVALS/ NAME1, NAME2, NAMEO, CLAS1, CLAS2, CLASO
LOCAL END
      PROGRAM BDAPL
C-----------------------------------------------------------------------
C! Applies BD table of one file to another
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2013-2015
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
C   BDAPL applies a BD table of one file to another writing a 3rd
C   UV data set
C   Adverbs:
C      INNAME      UV data set to calibrate: name
C      INCLASS     UV data set to calibrate: class
C      INSEQ       UV data set to calibrate: seq #
C      INDISK      UV data set to calibrate: disk
C      IN2NAME     UV data set with BD table: name
C      IN2CLASS    UV data set with BD table: class
C      IN2SEQ      UV data set with BD table: seq #
C      INDISK      UV data set with BD table: class
C      BLVERS      BD table version #
C      OUTNAME     Output UV data set: name
C      OUTCLASS    Output UV data set: class
C      OUTSEQ      Output UV data set: seq #
C      OUTDISK     Output UV data set: disk
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      REAL      BDVALS(2)
      LONGINT   PBDVAL
      INTEGER   IRET, SCRTCH(256)
      INCLUDE 'BDAPL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'BDAPL '/
C-----------------------------------------------------------------------
C                                       Initialize
      CALL BDAPLI (PRGM, BDVALS, PBDVAL, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Edit tables
      CALL BDAPLD (NUMPOL, NUMFRQ, NUMIF, NUMANT, BDVALS(1+PBDVAL),
     *   IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Add history to output
      CALL BDAPLH
C                                       Close down files, etc
 995  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE BDAPLI (PRGM, BDVALS, PBDVAL, IRET)
C-----------------------------------------------------------------------
C   Starts up BDAPL, sets common variables, allocates big memory and
C   fills it with the BD table values
C   Inputs:
C      PRGM     C*6    Program name
C   Outputs:
C      BDVALS   R(*)   Address for large memory to be based
C      PBDVAL   L      Pointer into BDVALS for large memory
C      IRET     I      Error code
C-----------------------------------------------------------------------
      CHARACTER PRGM*(*)
      REAL      BDVALS(*)
      LONGINT   PBDVAL
      INTEGER   IRET
C
      CHARACTER TYPE*2, STAT*4, BLANK*6
      INTEGER   SCRTCH(256), NPARM, IROUND, IERR, IVER, LUN, LUNTMP,
     *   BDBUFF(512), BDKOLS(10), BDNUMV(10), IBDRNO, NEED, INCX
      INCLUDE 'INCS:PUVD.INC'
      REAL      BNDPAS(2*MAXCIF)
      INCLUDE 'BDAPL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 23
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAM1, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAM1, NAME1)
      CALL H2CHR (12, 1, XNAM2, NAME2)
      CALL H2CHR (12, 1, XNAMO, NAMEO)
      CALL H2CHR (6, 1, XCLAS1, CLAS1)
      CALL H2CHR (6, 1, XCLAS2, CLAS2)
      CALL H2CHR (6, 1, XCLASO, CLASO)
      SEQ1 = XSEQ1 + 0.1
      SEQ2 = XSEQ2 + 0.1
      SEQO = XSEQO + 0.1
      DISK1 = XDISK1 + 0.1
      DISK2 = XDISK2 + 0.1
      DISKO = XDISKO + 0.1
      VERS = IROUND (XVERS)
C                                       Find input
      TYPE = 'UV'
      CNO1 = 1
      CALL CATDIR ('SRCH', DISK1, CNO1, NAME1, CLAS1, SEQ1, TYPE,
     *   NLUSER, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAME1, CLAS1, SEQ1, DISK1, NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISK1, CNO1, CATBLK, 'READ', SCRTCH, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK1
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0
      CALL COPY (256, CATBLK, CATOLD(1,1))
C                                       BD file data set
      IF (SEQ2.LE.0) SEQ2 = SEQ1
      IF (DISK2.LE.0) DISK2 = DISK1
      IF (NAME2.EQ.' ') NAME2 = NAME1
      IF (CLAS2.EQ.' ') CLAS2 = CLAS1
      CNO2 = 1
      CALL CATDIR ('SRCH', DISK2, CNO2, NAME2, CLAS2, SEQ2, TYPE,
     *   NLUSER, STAT, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET, NAME2, CLAS2, SEQ2, DISK2, NLUSER
         GO TO 990
         END IF
C                                       Read old CATBLK and mark 'READ'
      CALL CATIO ('READ', DISK2, CNO2, CATBLK, 'READ', SCRTCH, IRET)
      IF ((IRET.GT.0) .AND. (IRET.LT.5)) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISK2
      FCNO(NCFILE) = CNO2
      FRW(NCFILE) = 0
C                                       check on BD table
      CALL FNDEXT ('BD', CATBLK, IVER)
      IF (IVER.LE.0) THEN
         MSGTXT = 'IN2NAME DOES NOT HAVE A BD TABLE'
         IRET = 10
         GO TO 990
         END IF
      IF ((VERS.LE.0) .OR. (VERS.GT.IVER)) VERS = IVER
      CALL COPY (256, CATBLK, CATOLD(1,2))
C                                       channel, IF structure
      CALL COPY (256, CATOLD(1,1), CATBLK)
      CALL UVPGET (IRET)
C                                       open BD file
      LUN= LUNTMP (1)
      CALL BDINI ('READ', BDBUFF, DISK2, CNO2, VERS, CATOLD(1,2), LUN,
     *   IBDRNO, BDKOLS, BDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, IRET)
      IF (CATBLK(KINAX+JLOCS).LT.NUMPOL) THEN
         MSGTXT = 'INPUT FILE HAS TOO FEW POLARIZATIONS'
         IRET = 9
         GO TO 990
         END IF
      IF (CATBLK(KINAX+JLOCF).NE.NUMFRQ) THEN
         MSGTXT = 'INPUT FILE DOES NOT MATCH NUMBER CHANNELS'
         IRET = 9
         GO TO 990
         END IF
      IF (CATBLK(KINAX+JLOCIF).NE.NUMIF) THEN
         MSGTXT = 'INPUT FILE DOES NOT MATCH NUMBER IFS'
         IRET = 9
         GO TO 990
         END IF
C                                       allocate memory
      NEED = NUMANT * NUMANT * NUMFRQ * NUMIF * NUMPOL
      NEED = (NEED - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', TSKNAM, NEED, BDVALS, PBDVAL, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'COULD NOT GET NEEDED DYNAMIC MEMORY'
         GO TO 990
         END IF
      NEED = NEED * 1024
      CALL RFILL (NEED, FBLANK, BDVALS(1+PBDVAL))
C                                       read BD table
      CALL BDREAD (NUMPOL, NUMANT, NUMIF, NUMFRQ, BDVALS(1+PBDVAL),
     *   BNDPAS, BDBUFF, BDKOLS, BDNUMV, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       create output file
C                                       Put new values in CATBLK.
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
      CALL MAKOUT (NAME1, CLAS1, SEQ1, BLANK, NAMEO, CLASO, SEQO)
      CALL CHR2H (12, NAMEO, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASO, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQO
      CNOO = 1
      CALL UVCREA (DISKO, CNOO, SCRTCH, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'CREATING OUTPUT FILE'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
C                                       Copy any header keywords
      CALL KEYCOP (DISK1, CNO1, DISKO, CNOO, IRET)
      IF (IRET.NE.0) GO TO 999
      SEQO = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' GETING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',I3,
     *   ' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1010 FORMAT ('BDAPLI ERROR',I5,' ON ',A)
      END
      SUBROUTINE BDREAD (NUMPOL, NUMANT, NUMIF, NUMFRQ, BDVALS, BNDPAS,
     *   BDBUFF, BDKOLS, BDNUMV, IRET)
C-----------------------------------------------------------------------
C   Reads in the bd table and closes it at the end
C   Inputs:
C      NUMPOL   I      Number polarizations (1, 2)
C      NUMANT   I      Max antenna number
C      NUMIF    I      Number IFs
C      NUMFRQ   I      Number spectral channels
C      BDKOLS   I(*)   Column pointers
C      BDNUMV   I(*)   Column sizes
C   In/Out
C      BDVALS   R(*)   BD table array
C      BDBUFF   I(*)   BD table IO control block
C   Output:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NUMPOL, NUMANT, NUMIF, NUMFRQ, BDBUFF(*), BDKOLS(*),
     *   BDNUMV(*), IRET
      REAL      BDVALS(NUMFRQ,NUMIF,NUMANT,NUMANT,*),
     *   BNDPAS(2,NUMFRQ,NUMIF,*)
C
      INTEGER   IBDRNO, IROW, NROWS, LP, LI, LF, SOURID, SUBA, ANT1,
     *   ANT2, FREQID
      REAL      TIME, DBG(2,256,2)
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NROWS = BDBUFF(5)
      DO 100 IROW = 1,NROWS
         IBDRNO = IROW
         CALL TABBD ('READ', BDBUFF, IBDRNO, BDKOLS, BDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, TIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *      BNDPAS, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING BD TABLE'
            GO TO 990
            END IF
         CALL RCOPY (1024, BNDPAS, DBG)
         DO 50 LP = 1,NUMPOL
            DO 40 LI = 1,NUMIF
               DO 30 LF = 1,NUMFRQ
                  BDVALS(LF,LI,ANT1,ANT2,LP) = BNDPAS(1,LF,LI,LP)
                  BDVALS(LF,LI,ANT2,ANT1,LP) = BNDPAS(2,LF,LI,LP)
 30               CONTINUE
 40            CONTINUE
 50         CONTINUE
 100     CONTINUE
      CALL TABBD ('CLOS', BDBUFF, IBDRNO, BDKOLS, BDNUMV, NUMIF,
     *   NUMFRQ, NUMPOL, TIME, SOURID, SUBA, ANT1, ANT2, FREQID,
     *   BNDPAS, IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BDREAD ERROR:',I5,' ON ',A)
      END
      SUBROUTINE BDAPLD (NP, NF, NI, NA, BDVALS, IRET)
C-----------------------------------------------------------------------
C   BPAPLD applies the BD corrections to the data writing out the new
C   UV data set
C   Inputs:
C      NP       I      Number polarizations (1, 2)
C      NF       I      Number spectral channels
C      NI       I      Number IFs
C      NA       I      Max antenna number
C      BDVALS   R(*)   BD table values (f,i,a1,a2,p)
C   Outputs:
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NP, NF, NI, NA, IRET
      REAL      BDVALS(NF,NI,NA,NA,*)
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'BDAPL.INC'
      INTEGER   LUNI, INDI, LUNO, INDO, NCOPY, NCORO, ILENBU, VO, BO,
     *   IPTRO, NIOUT, NIOLIM, JBUFSZ, NUMVIS, ILOCWT, RNXRET, IBIND,
     *   KBIND, INIO, I, IPTRI
      REAL      BUFFI(UVBFSS), BUFFO(UVBFSS), UBUFF(UVBFSS)
      CHARACTER FILE*48
      LOGICAL   T, F, ISCOMP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T,F /.TRUE.,.FALSE./
      DATA LUNI, LUNO /28,29/
C-----------------------------------------------------------------------
C                                       visibility file
      CALL ZPHFIL ('UV', DISK1, CNO1, 1, FILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISK1, FILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT UV'
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CNOO, 1, FILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, FILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT UV'
         GO TO 990
         END IF
C                                       Find weight and scale.
      ISCOMP = CATBLK(KINAX).EQ.1
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            IRET = 9
            GO TO 990
            END IF
         END IF
C                                       Init vis file for write
      NCOPY = LREC - NRPARM
      NCORO = (LREC - NRPARM) / CATBLK(KINAX)
      ILENBU = 0
      VO = 0
      BO = 1
      JBUFSZ = UVBFSS  * 2
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFFO, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT UV'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU,
     *   JBUFSZ, BUFFI, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT INPUT UV'
         GO TO 990
         END IF
      NUMVIS = 0
C                                       make an index table
      CALL RNXGET (DISK1, CNO1, CATOLD(1,1))
      CALL RNXINI (DISKO, CNOO, CATBLK, RNXRET)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFFI, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READ INPUT UV DATA'
         GO TO 990
         END IF
      IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 190 I = 1,INIO
C                                       move random parameters
            BUFFI(IPTRI+ILOCU) = BUFFI(IPTRI+ILOCU) * UVSCAL
            BUFFI(IPTRI+ILOCV) = BUFFI(IPTRI+ILOCV) * UVSCAL
            BUFFI(IPTRI+ILOCW) = BUFFI(IPTRI+ILOCW) * UVSCAL
            CALL RCOPY (NRPARM, BUFFI(IPTRI), BUFFO(IPTRO))
C                                       compressed
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORO, BUFFI(IPTRI+NRPARM),
     *            BUFFI(IPTRI+ILOCWT), UBUFF)
               CALL SCALIT (NF, NI, NA, NP, BDVALS, BUFFO(IPTRO), UBUFF,
     *            IRET)
               IF (IRET.EQ.0) CALL ZUVPAK (NCORO, UBUFF,
     *            BUFFO(IPTRO+ILOCWT), BUFFO(IPTRO+NRPARM))
C                                       Decide if it's kept, select
            ELSE
               CALL SCALIT (NF, NI, NA, NP, BDVALS, BUFFO(IPTRO),
     *            BUFFI(IPTRI+NRPARM), IRET)
               IF (IRET.EQ.0) CALL RCOPY (NCOPY, BUFFI(IPTRI+NRPARM),
     *            BUFFO(IPTRO+NRPARM))
               END IF
            NUMVIS = NUMVIS + 1
            IF (MOD(NUMVIS-1,50000).EQ.0) THEN
               WRITE (MSGTXT,1010) NUMVIS
               CALL MSGWRT (2)
               END IF
            NIOUT = NIOUT + 1
C                                       update NX table
            CALL RNXUPD (BUFFO(IPTRO), RNXRET)
            IPTRI = IPTRI + LREC
            IPTRO = IPTRO + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFFO, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'WRIT OUTPUT UV'
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFO, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FINISH OUTPUT UV'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = NUMVIS
      CALL UCMPRS (NVIS, DISKO, CNOO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BDAPLD ERROR',I5,' ON ',A)
 1010 FORMAT ('At visibility',I10)
      END
      SUBROUTINE SCALIT (NF, NI, NA, NP, BDVALS, RPARM, VIS, IRET)
C-----------------------------------------------------------------------
C   SCALIT applies the scaling to a visibility sample
C   Inputs:
C      NP       I      Number polarizations (1, 2)
C      NF       I      Number spectral channels
C      NI       I      Number IFs
C      NA       I      Max antenna number
C      BDVALS   R(*)   BD table values (f,i,a1,a2,p)
C      RPARM    R(*)   Input random parameters
C   in/out
C      VIS      R(*)   Output data
C   Outputs
C      IRET     I      Error code
C-----------------------------------------------------------------------
      INTEGER   NF, NI, NA, NP, IRET
      REAL      BDVALS(NF,NI,NA,NA,*), RPARM(*), VIS(3,*)
C
      INTEGER   IA1, IA2, LI, LF, LP, INCX, LNCIF, LNCF, LNCS, IND
      REAL      BASL, CR, CI, TR, TI
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (ILOCB.GE.0) THEN
         BASL = RPARM(1+ILOCB)
         IA1 = BASL / 256.0 + 0.01
         IA2 = (BASL - IA1*256) + 0.01
      ELSE
         IA1 = RPARM(1+ILOCA1) + 0.1
         IA2 = RPARM(1+ILOCA2) + 0.1
         END IF
      INCX = CATBLK(KINAX)
      LNCIF = INCIF / INCX
      LNCF = INCF / INCX
      LNCS = INCS / INCX
      IF (IA1.NE.IA2) THEN
C                                       blank improper
         IF ((IA1.LE.0) .OR. (IA2.LE.0) .OR. (IA1.GT.NA) .OR.
     *      (IA2.GT.NA)) THEN
            DO 30 LP = 1,NP
               DO 20 LI = 1,NI
                  IND = 1 + (LI - 1) * LNCIF + (LP - 1) * LNCS
                  DO 10 LF = 1,NF
                     VIS(3,IND) = -ABS(VIS(3,IND))
                     IND = IND + LNCF
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
C                                       cal
         ELSE
            DO 80 LP = 1,NP
               DO 70 LI = 1,NI
                  IND = 1 + (LI - 1) * LNCIF + (LP - 1) * LNCS
                  DO 60 LF = 1,NF
                     IF (VIS(3,IND).GT.0.0) THEN
                        CR = BDVALS(LF,LI,IA1,IA2,LP)
                        CI = -BDVALS(LF,LI,IA2,IA1,LP)
                        TR = CR * VIS(1,IND) + CI * VIS(2,IND)
                        TI = CR * VIS(2,IND) - CI * VIS(1,IND)
                        VIS(1,IND) = TR
                        VIS(2,IND) = TI
                        CR = CR * CR + CI * CI
                        IF (CR.GT.0.0) THEN
                           VIS(3,IND) = VIS(3,IND) / CR
                        ELSE
                           VIS(3,IND) = 0.0
                           END IF
                        END IF
                     IND = IND + LNCF
 60                  CONTINUE
 70               CONTINUE
 80            CONTINUE
            END IF
         END IF
C
 999  RETURN
      END
      SUBROUTINE BDAPLH
C-----------------------------------------------------------------------
C   writes HIstory and copies tables
C-----------------------------------------------------------------------
C
      CHARACTER HILINE*72, NOTTYP(2)*2
      INTEGER   LUN1, LUN2, IERR, NONOT, BUFF1(256), BUFF2(256)
      INCLUDE 'BDAPL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /2, 'NX','BD'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISK1, DISKO, CNO1, CNOO, CATBLK,
     *   BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAME1, CLAS1, SEQ1, DISK1, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      IF ((DISK1.NE.DISK2) .OR. (CNO1.NE.CNO2)) THEN
         CALL HENCO2 (TSKNAM, NAME2, CLAS2, SEQ2, DISK2, LUN2, BUFF2,
     *      IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
      CALL HENCOO (TSKNAM, NAMEO, CLASO, SEQO, DISKO, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       BD table version written
      WRITE (HILINE,1010) TSKNAM, VERS
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Close HI file
 190  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
 200  CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISK1, DISKO, CNO1,
     *   CNOO, CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR IN COPYING TABLES TO OUTPUT FILE'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, CNOO, DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'BDAPLH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK
      CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I3,' COPYING TO NEW HI FILE')
 1010 FORMAT (A6,'BLVERS = ',I3,' / BD table used')
      END


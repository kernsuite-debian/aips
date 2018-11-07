LOCAL INCLUDE 'UVDI1.INC'
C                                       Local include for UVDI1
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   CATIN(256), SEQIN, SEQ2IN, SEQOUT, DISKIN, DIS2IN,
     *   DISKO, NUMHIS, JBUFSZ, CAT1(256), CAT2(256), CAT3(256), NANT,
     *   ILOCWT, MTYPE, NVOUT, LUNI, LUNO, INDI, INDO, IND, CNO1, CNO2,
     *   CNOO, NRPAR1
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAM2I(3), XCL2IN(2), XNAMOU(3),
     *   XCLAOU(2), CATH1(256), CATH2(256), CATH3(256), CATIH(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAM2IN*12, CLA2IN*6, NAMOUT*12,
     *   CLAOUT*6, HISCRD(10)*64, ONAME*48
      REAL      XSIN, XDISIN, XS2IN, XDI2IN, XSOUT, XDISO, XINC, YINC,
     *   ZINC, BUFF1(UVBFSS), BUFF2(UVBFSS), CATR1(256), CATR2(256),
     *   CATR3(256), CATIR(256)
      DOUBLE PRECISION CATD1(128), CATD2(128), CATD3(128), CATID(128)
      LOGICAL   LTB, ISCOMP
C                                       ACSIZE = size of accum. array
      INTEGER ACSIZE
      PARAMETER (ACSIZE = 2500000)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /OLDHDR/ CATIN, LTB, NANT
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAM2I, XCL2IN,
     *   XS2IN, XDI2IN, XNAMOU, XCLAOU, XSOUT, XDISO, XINC, YINC,
     *   ZINC
      COMMON /PVALS/ SEQIN, SEQ2IN, SEQOUT,  DISKIN, DIS2IN, DISKO,
     *   NUMHIS, ILOCWT, MTYPE, NVOUT, ISCOMP, LUNI, LUNO, INDI, INDO,
     *   IND, NRPAR1, CNO1, CNO2, CNOO
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAM2IN, CLA2IN, NAMOUT, CLAOUT,
     *   HISCRD, ONAME
      COMMON /MAP2HD/ CAT1, CAT2, CAT3
      EQUIVALENCE (CAT1, CATR1, CATH1, CATD1)
      EQUIVALENCE (CAT2, CATR2, CATH2, CATD2)
      EQUIVALENCE (CAT3, CATR3, CATH3, CATD3)
      EQUIVALENCE (CATIN, CATIR, CATIH, CATID)
LOCAL END
      PROGRAM UVDI1
C-----------------------------------------------------------------------
C! Subtract one time UV data from the other UV data
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2007-2009, 2011, 2015-2016
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
C   UVDI1 subtracts the given one time UV data from the other UV data
C   The one time UV data can be created averaging UV data by time using
C   very large averaging time at the task UVAVG.
C   UVAVG (OPCODE = 'SUBT') prepares such one time UV data and then
C   subtracts it from the original UV data.
C   UVDI1 picks up the one time UV data from somewhere.
C
C   If XINC if greater than 1, only every XINC'th group of output
C   records (eg all baselines for an average interval) will be written.
C
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of input UV data.
C   INCLASS        CLAIN         Class of input UV data.
C   INSEQ          SEQIN         Seq. of input UV data.
C   INDISK         DISKIN        Disk number of input UV data.
C   IN2NAME        NAM2IN        Name of input UV data.
C   IN2CLASS       CLA2IN        Class of input UV data.
C   INSEQ          SEQ2IN        Seq. of input UV data.
C   INDISK         DIS2IN        Disk number of input UV data.

C   OUTNAME        NAMOUT        Name of the output uv file.
C   OUTCLASS       CLAOUT        Class of the output uv file.
C   OUTSEQ         SEQOUT        Seq. number of output uv data.
C   OUTDISK        DISKO         Disk number of the output file.
C   XINC           XINC          Write only XINC'th output recs.
C   YINC           YINC          Integration time (sec) min=0.2
C   ZINC           ZINC          Input averaging time (sec)
C   UVDI1 Programmer L.R. Kogan, Jan 2007.
C-----------------------------------------------------------------------
      INTEGER   IRET
      REAL      YINCUR
      CHARACTER PRGM*6
      INCLUDE 'UVDI1.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVDI1 '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVAVIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
      YINCUR = YINC
      DO 100 IND = 1,2
C                                       IND=1 reading the one time UV
C                                       data using subroutine UVAVDO
C                                       averaging over the large time
C                                       (10days) and store it at VBUFF1
         IF (IND.EQ.1) THEN
            YINC = 10.0
C                                       subtracting the found complex
C                                       number at IND=1 (VBUFF1)
C                                       from the regular averaged data
C                                       using averaging with given YINC
         ELSE
            YINC = YINCUR
            END IF
         CALL UVAVUV (IRET)
         IF (IRET.NE.0) GO TO 990
 100     CONTINUE
C
      CALL UVAVHI
C                                       close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVAVIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVAVIN gets input parameters for UVDI1 and creates an output file
C   if necessary.
C   Inputs:  PRGM   C*6      Task name
C   Output:  JERR   I        Error code: quit if > 0.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGM*6, UTYPE*2
      INTEGER   IROUND, JERR, NPARM, IERR,  NUMAN(513), LUN, ITEMP,
     *   NIF1, NIF2, NS1, NS2, NF1, NF2, WBUFF(256)
      REAL      FACT, RTEMP
      LOGICAL   T, F
      LOGICAL   TABLE, FITASC, IS1MS, IS2MS
      INCLUDE 'UVDI1.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 24
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQ2IN = IROUND (XS2IN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DIS2IN = IROUND (XDI2IN)
      DISKO = IROUND (XDISO)
C                                       average time in days
      YINC = MAX (0.2, YINC) / 86400.0
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAM2I, NAM2IN)
      CALL H2CHR (6, 1, XCL2IN, CLA2IN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Open the first file
C                                       and get catalog at CAT1
      CNO1 = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNO1, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
C                                       Error finding file.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
C
      CALL CATIO ('READ', DISKIN, CNO1, CAT1, 'READ', BUFF1, IERR)
C                                       Error copying CATBLK.
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                        Update /CFILE/
      NCFILE = NCFILE + 1
C                                       NCFILE = 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNO1
      FRW(NCFILE) = 0

C                                       See if multisource
      LUN = 29
      CALL MULSDB (CAT1, IS1MS)
      IF (IS1MS) THEN
         CALL ISTAB ('SU', DISKIN, CNO1, 1, LUN, BUFF1, TABLE, IS1MS,
     *      FITASC, IERR)
         IS1MS = IS1MS .AND. (IERR.EQ.0)
         END IF
C                                       Open the second file
C                                       and get catalog at CAT2
      CNO2 = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DIS2IN, CNO2, NAM2IN, CLA2IN, SEQ2IN,
     *   UTYPE, NLUSER, STAT, WBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAM2IN, CLA2IN, SEQ2IN, DIS2IN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DIS2IN, CNO2, CAT2, 'READ', WBUFF, IERR)
C                                       Error copying CATBLK.
      IF (IERR.GT.1) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Copy CAT2 => CATBLK
      CALL COPY (256, CAT2, CATBLK)
      CALL UVPGET (JERR)
C                                       Number of stokes, IF, F for
C                                       the second UV data
      NS2 =  CAT2(KINAX + JLOCS)
      NIF2 = CAT2(KINAX + JLOCIF)
      NF2  = CAT2(KINAX + JLOCF)
C                                        Update /CFILE/
      NCFILE = NCFILE + 1
C                                       NCFILE = 2
      FVOL(NCFILE) = DIS2IN
      FCNO(NCFILE) = CNO2
      FRW(NCFILE) = 0
C                                       See if multisource
      CALL MULSDB (CAT2, IS2MS)
      IF (IS2MS) THEN
         CALL ISTAB ('SU', DIS2IN, CNO2, 1, LUN, BUFF1, TABLE, IS2MS,
     *      FITASC, IERR)
         IS2MS = IS2MS .AND. (IERR.EQ.0)
         END IF
C                                       Can't subtract
      IF (IS2MS)  THEN
         JERR = 2
         MSGTXT = 'THE SECOND UV FILE MUST BE A SINGLE SOURCE FILE'
         GO TO 990
         END IF
C                                       Copy old CATBLK
      CALL COPY (256, CAT1, CATBLK)
      CALL COPY (256, CATBLK, CATIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Number of stokes, IF, F for
C                                       the first UV data
      NS1 =  CAT1(KINAX + JLOCS)
      NIF1 = CAT1(KINAX + JLOCIF)
      NF1  = CAT1(KINAX + JLOCF)
C                                       Stokes numbers identical?
      IF (NS1.NE.NS2) THEN
         JERR = 2
         MSGTXT = 'CANNOT SUBTRACT: DIFFERENT NUMBER STOKES'
         GO TO 990
         END IF
C                                       IF numbers identical?
      IF (NIF1.NE.NIF2) THEN
         JERR = 2
         MSGTXT = 'CANNOT SUBTRACT: DIFFERENT NUMBER IFS'
         GO TO 990
         END IF
C                                       F numbers identical?
      IF (NF1.NE.NF2) THEN
         JERR = 2
         MSGTXT = 'CANNOT SUBTRACT: DIFFERENT NUMBER FREQUENCIES'
         GO TO 990
         END IF
C
      LTB = F
      NANT = 2
C                                       Check if sort order 'TB' or 'BT'
      IF ((ISORT.NE.'BT') .AND. (ISORT.NE.'TB')) THEN
         WRITE (MSGTXT,1050) ISORT
         JERR = 5
         GO TO 990
         END IF
C                                       If 'TB', see if accum. buffers
C                                       will be large enough
      IF (ISORT.EQ.'TB') THEN
         LTB = T
C                                       Get number of antennas from AN
C                                       file.
         LUN = 29
         CALL GETNAN (DISKIN, CNO1, CATIN, LUN, BUFF1, NUMAN, IERR)
         NANT = NUMAN(2)
C                                       If failed, assume 28.
         IF ((IERR.NE.0) .OR. (NANT.LE.0)) THEN
            WRITE (MSGTXT,1052)
            CALL MSGWRT (6)
            NANT = 28
            END IF
C                                       Allow a maximum of 50 antennas
         IF ((LTB) .AND. (NANT.GT.50)) THEN
            WRITE (MSGTXT,1053)
            JERR = 10
            GO TO 990
            END IF
         END IF
C                                       Set def. outname to inname 1
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CNOO = 1
C
      FRW(NCFILE+1) = 3
      JERR = 4
C                                       Adjust output size if ZINC is
C                                       non-zero
      IF ((ZINC.GT.0.0) .AND. (YINC.GT.0.0)) THEN
         FACT = ZINC / (YINC * 86400.0)
         ITEMP = CATBLK(KIGCN)
         RTEMP = ITEMP
         RTEMP = RTEMP * (FACT+0.15)
         CATBLK(KIGCN) = IROUND(RTEMP)
         END IF
C
      NVOUT = CATBLK(KIGCN)
      CALL UVCREA (DISKO, CNOO, BUFF1, IERR)
      IF ((ZINC.GT.0.0) .AND. (YINC.GT.0.0)) CATBLK(KIGCN) = ITEMP
C                                       output catblk modified by UVCREA
      CALL COPY (256, CATBLK, CAT3)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1055) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         ELSE IF ((CNOO.NE.CNO1) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
C                                       Recover existing CATBLK
         ELSE
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CNOO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
               END IF
            END IF
         END IF
C                                       NCFILE = 3
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CNOO
      FRW(NCFILE) = 2
      SEQOUT = CATBLK(KIIMS)
      JERR = 0
C                                       copy header keywords
      CALL KEYCOP (DISKIN, CNO1, DISKO, CNOO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('UVAVIN: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' READING CATALOG HEADER FOR INPUT FILE')
 1050 FORMAT ('UVAVIN: SORT ORDER ',A2,' NOT BT OR TB AS REQUIRED')
 1052 FORMAT ('UVAVIN: COULD NOT GET NUMBER OF ANTENNAS FROM AN FILE',
     *   ' - ASSUME 28')
 1053 FORMAT ('UVAVIN: MORE THAN 50 ANTENNAS IN AN FILE - SORT TO BT')
 1055 FORMAT ('UVAVIN: ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('UVAVIN: MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVAVIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVAVUV (IRET)
C-----------------------------------------------------------------------
C   UVAVUV sends uv data one point at a time to the average/merge
C   routine and then writes the modified data if requested.
C   Input in common:
C      ISCOMP  L  If true data is compressed
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER PHNAME(6)*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LRECO, NIOLIM, NIOUT, KBIND,
     *   IA1, IA2, IBIND, ILENBU, I, VO, BO, NUMVIS, XCOUNT, NCORR
      LOGICAL   T, F
      INCLUDE 'UVDI1.INC'
      REAL      CBUFF(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      LUNI = 16
      LUNO = 17
      IF (IND.EQ.1) THEN
C                                       Copy CAT of the second file
         CALL COPY (256, CAT2, CATBLK)
C                                       Compressed data?
         ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
         IF (ISCOMP) THEN
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH2(KHPTP),
     *         ILOCWT, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
               IRET = 9
               GO TO 990
               END IF
            END IF
C                                       Get uv header info.
         CALL UVPGET (IRET)
C                                       store NRPARM at IND = 1
         NRPAR1 = NRPARM
C                                       Number of visibilities in the
C                                       second input file.
         NCORR = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Open and init for read
C                                       the second file
         CALL ZPHFIL ('UV', DIS2IN, CNO2, 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DIS2IN, PHNAME, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
C                                       second pass
      ELSE
C                                       Copy CAT of the output
         CALL COPY (256, CAT3, CATBLK)
C                                       Compressed data?
         ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
         IF (ISCOMP) THEN
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH3(KHPTP),
     *         ILOCWT, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
               IRET = 9
               GO TO 990
               END IF
            END IF
C                                       Get uv header info.
         CALL UVPGET (IRET)
C                                       Number of visibilities in the
C                                       first input and output file.
         NCORR = (LREC - NRPARM) / CATBLK(KINAX)
C                                       Open and init for read
C                                       the first file
         CALL ZPHFIL ('UV', DISKIN, CNO1, 1, PHNAME, IRET)
         CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET
            GO TO 990
            END IF
C                                       Open vis file for write
C                                       for IND=2 only
         CALL ZPHFIL ('UV', DISKO, CNOO, 1, ONAME, IRET)
         CALL ZOPEN (LUNO, INDO, DISKO, ONAME, T, F, F, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
            END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
         LRECO = LREC
         ILENBU = 0
         CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU,
     *      JBUFSZ,BUFF2, BO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         IPTRO = KBIND
         NIOUT = 0
         NIOLIM = ILENBU
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
C                                       Init counters.
      NUMVIS = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                        Test for end of data
      ELSE IF (INIO.GT.0) THEN
C                                        Prepare to call UVAVDO
C                                        Loop through data in buffer
         DO 190 I = 1,INIO
            IPTRI = IBIND + (I-1)*LREC
            IF (ILOCB.GE.0) THEN
               IA2 = BUFF1(IPTRI+ILOCB) + 0.1
               IA1 = IA2 / 256
               IA2 = IA2 - IA1*256
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                       Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORR, BUFF1(IPTRI+NRPARM),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL UVAVDO (NUMVIS, BUFF1(IPTRI+ILOCT), IA1, IA2, CBUFF,
     *            BUFF1(IPTRI), IRET, NIOLIM, LRECO, IPTRO, NIOUT,
     *            XCOUNT)
C                                       Un compressed data
            ELSE
               CALL UVAVDO (NUMVIS, BUFF1(IPTRI+ILOCT), IA1, IA2,
     *            BUFF1(IPTRI+NRPARM), BUFF1(IPTRI), IRET, NIOLIM,
     *            LRECO, IPTRO, NIOUT, XCOUNT)
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
               END IF
 190        CONTINUE
         GO TO 100
         END IF
C                                       Final call to UVAVDO.
      NUMVIS = -1
      CALL UVAVDO (NUMVIS, BUFF1(IPTRI+ILOCT), IA1, IA2,
     *   BUFF1(IPTRI+NRPARM), BUFF1(IPTRI), IRET, NIOLIM, LRECO, IPTRO,
     *   NIOUT, XCOUNT)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Close input file
      CALL ZCLOSE (LUNI, INDI, IRET)
C                                       Compress and close output file.
      IF (IND.EQ.2) THEN
         NVIS = XCOUNT
         CALL ZCLOSE (LUNO, INDO, IRET)
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('UVAVUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('UVAVUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UVAVUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVAVUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVAVUV: UVAVDO ERROR',I3)
      END
      SUBROUTINE UVAVHI
C-----------------------------------------------------------------------
C   UVAVHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'UVDI1.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2, T /27,28, .TRUE./
      DATA NONOT, NOTTYP /1,'NX'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, CNO1, CNOO, CATBLK, BUFF1,
     *   BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCO2 (TSKNAM, NAM2IN, CLA2IN, SEQ2IN, DIS2IN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any other history
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, CNO1, CNOO,
     *   CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1019)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, CNOO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVAVHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
 1019 FORMAT ('UVAVHI: ERROR COPYING TABLES')
      END
      SUBROUTINE UVAVDO (NUMVIS, T, IA1, IA2, VIS, RPARM, IRET, NIOLIM,
     *   LRECO, IPTRO, NIOUT, XCOUNT)
C-----------------------------------------------------------------------
C   UVAVDO averages a uv data set in time.
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed but allows any operations to be completed.
C                      Data sent back will be wirtten to output file.
C      T          R    Time in days since 0 IAT on the first day for
C                      which there is data.
C      IA1        I    First antenna number
C      IA2        I    Second antenna number
C      RPARM(*)   I    Random parameter array which includes U,V,W etc
C                      but also any other random parameters.
C      VIS(3,*)   R    Vis data in order real, imaginary, weight (Jy)
C      NIOLIM     I    Maximum number or records per buffer.
C      IPTRO      I    Output pointer - initialize before first call.
C      NIOUT      I    Number of records in buffer.  Init before first
C                                 call
C      XCOUNT     I    Output record counter.  Init before first call.
C   Inputs from COMMON
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      CATBLK(256)I    Catalog header record.
C   Output:
C      RPARM      R    Modified random parameter array. NB U,V,W, time
C                      and baseline should not be modified in RPARM
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C   History records
C      CATBLK     I    Catalog header block
C   Programmer: R. C. Walker  Feb. 1984, L.R. Kogan Jan. 2007
C-----------------------------------------------------------------------
C                                        Declares for call.
      INTEGER   IA1, IA2, IRET, NIOLIM, IPTRO, NIOUT, LRECO, IP, I2TMP
      REAL      T, VIS(3,*), RPARM(*)
      INTEGER   NUMVIS, XCOUNT
C
      INTEGER   ANOTA(20)
      CHARACTER OUTRAN(20)*8
C                                       Main declares.
      INTEGER MXINDX
      INTEGER KBUFF1, INDEX1
C                                       Accumulation buffer
C                                       index size.
      PARAMETER (MXINDX = 1136)
      INTEGER   IIREC, IIREC2, ILEN, LUNSS, JERR, JRET, INDEX, IDAY,
     *   IBAS, BLCUR, KBAS, INDEX2, NIOFLS, INDEXV, I, KBIND, MCOR,
     *   NMCOR, LBAS, KBUFF, ITIME, IWORK(MXINDX), ITT, NCOPY, ANVER,
     *   CURSOU, LSTSOU, CURFQ, LSTFQ, NFQID, KREC, KFQID, IFQTRA(64),
     *   K, KLIM
      REAL      DIV, WGT, TLAST, CT, TCHK, TYINC, OPARM(20),
     *   TIMES(3,MXINDX), WORK(2,MXINDX)
      DOUBLE PRECISION X8, NXINC, XINC8
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'UVDI1.INC'
      REAL      RESULT(UVBFSS)
C                                        For accumulation.
C                                        enough for 32 chan, 3 words/ch.
C                                        27 stations plus 8 PARMS +
C                                        1 word for a PARMS weight.
C                                        Space will be reconfigured for
C                                        each data set.
C                                        Adequacy was checked in UVAVIN
      REAL      VBUFF(ACSIZE), DTUTC
      REAL      VBUFF1(ACSIZE), VRE, VIM
      LOGICAL   ITPRT
      INTEGER   IIA1, IIA2
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE VBUFF, NCOPY, DTUTC, TLAST, BLCUR, MCOR, NMCOR, LBAS,
     *   XINC8, IIREC, IIREC2, ILEN, NXINC, LSTSOU, NFQID, IFQTRA, KLIM,
     *   ANOTA, TCHK, LSTFQ
      SAVE VBUFF1
      DATA LUNSS /27/
C-----------------------------------------------------------------------
      IRET = -1
C                                        Initial call
      IF (NUMVIS.EQ.1) THEN
         NUMHIS = 0
C                                       Number of visibilities in input
C                                       and output files.
         NCOPY = LRECO - NRPARM
C                                        Get data time - UTC
         DTUTC = 0.0
         ANVER = 1
         CALL ANTINI ('READ', VBUFF, FVOL(NCFILE), FCNO(NCFILE), ANVER,
     *      CATIN, LUNSS, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DTUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
         DTUTC = DTUTC / 86400.0
         CALL TABIO ('CLOS', 1, IANRNO, VBUFF, VBUFF, JERR)
C                                       warning/type random parameters
         CALL  LISRAN (OUTRAN, ANOTA)
C                                        Set counters etc.
         XCOUNT = 0
         TLAST = -1.0
         BLCUR = -1
         TCHK = -1.0
         MCOR = (LREC - NRPARM) / CATBLK(KINAX)
         NMCOR = MCOR * 3
         LBAS = 1
         IF (LTB) LBAS = NANT*(NANT+1)/2
         XINC8 = MAX (1.0, XINC)
         CURSOU = -1
         IF (ILOCSU.GE.0) CURSOU = RPARM(1+ILOCSU) + 0.5
         LSTSOU = CURSOU
         CURFQ = -1
         IF (ILOCFQ.GE.0) CURFQ = RPARM(1+ILOCFQ) + 0.5
         LSTFQ = CURFQ
C                                        Limit to accumulation buffer.
         KLIM = ACSIZE - (1 + NRPARM + NMCOR)
C                                        Reset FQ ID index
         NFQID = 0
         CALL FILL (64, 0, IFQTRA)
C                                        For sorts
         ILEN = 3
         I2TMP = MXINDX
         CALL RFILL (I2TMP, 0.0, TIMES)
C                                        Reset accumulation buffer
         I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
C                                        Set up to write first record
C                                        and avoid round off problems.
         NXINC = -0.999D0
         END IF
C                                        End of special first record
C                                        processing.
C                                        If in final call, skip tests.
      IF (NUMVIS.NE.-1) THEN
C                                        Be sure antenna numbers are
C                                        in the expected order.
         IF (IA1.GT.IA2) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (8)
            IRET = 7
            GO TO 999
            END IF
C                                        Get UT from data time
         CT = T - DTUTC
C                                        Get baseline.
         IBAS = 32768 * IA1 + IA2
C                                        Be sure of sort order.
         IF ((CT.LT.TCHK) .AND. (IBAS.EQ.BLCUR)) THEN
            WRITE (MSGTXT,1090)
            CALL MSGWRT (8)
            IRET = 4
            GO TO 999
            END IF
         END IF
C                                       Source Number, FQ id
      CURSOU = -1
      IF (ILOCSU.GE.0) CURSOU = RPARM(1+ILOCSU) + 0.5
      CURFQ = -1
      IF (ILOCFQ.GE.0) CURFQ = RPARM(1+ILOCFQ) + 0.5
C                                       Check if output needed.
      IF ((CT.GT.TLAST) .OR. (NUMVIS.EQ.-1) .OR.
     *   ((.NOT.LTB) .AND. (IBAS.NE.BLCUR)).OR.
     *   (CURSOU.NE.LSTSOU)) THEN
C                                        No output for first record.
         IF (NUMVIS.EQ.1) GO TO 180
C                                        Test if output should be
C                                        skipped.
            NXINC = NXINC + 1.D0
            IF ((XINC.GT.1.5) .AND. (MOD(NXINC,XINC8).GT.0.5D0))
     *         GO TO 128
C                                        Loop through the accumulation
C                                        buffer, extracting an
C                                        index of time and baseline
C                                        keys, which are later sorted.
               IIREC = LBAS * NFQID
C                                        Warning if MXINDX exceeded.
               IF (IIREC .GT. MXINDX) THEN
                  WRITE (MSGTXT, 1160) IIREC
                  CALL MSGWRT (8)
                  IRET = 10
                  GO TO 999
                  ENDIF
               DO 100 K = 1, IIREC
                  KREC = (K-1) * (1+NRPARM+NMCOR) + 1
                  IF (KREC .GT. KLIM) GO TO 100
                  WGT = VBUFF(KREC)
                  TIMES(3,K) = K
                  IF (WGT .GT. 0.0) THEN
                     TIMES(1,K) = VBUFF(KREC+ILOCT+1) / WGT
                     TIMES(2,K) = 4096.0 * IA1 + IA2
                  ELSE
                     TIMES(1,K) = 0.0
                     TIMES(2,K) = 0.0
                     ENDIF
  100             CONTINUE
C                                        Sort index of keys TIMES
C                                        to TB or BT order.
               IIREC2 = IIREC + 2
               IF (LTB) CALL OSORT (TIMES, IIREC, IIREC2, 1, 2,
     *               ILEN, WORK, IWORK, JERR)
               IF ((.NOT.LTB).AND.(IIREC.GT.1)) CALL OSORT (TIMES,
     *            IIREC, IIREC2, 2, 1, ILEN, WORK, IWORK, JERR)
               IF (JERR.GT.0) THEN
                  WRITE (MSGTXT,1104)
                  CALL MSGWRT (8)
                  IRET = 8
                  GO TO 999
                  END IF
C                                        Now write in time order.
               ITPRT = .TRUE.
               DO 126 ITIME = 1,IIREC
                  ITT  = IIREC - ITIME + 1
                  KREC = TIMES(3,ITT)
                  KBUFF = (KREC-1) * (1+NRPARM+NMCOR) + 1
                  IF (KBUFF .GT. KLIM) GO TO 126
C                                        Weight for RPARM.
C                                        Will be 0.0 if no valid data
                  WGT = VBUFF(KBUFF)
C                                        No data - no output
                  IF(WGT.LE.0.0) GO TO 124
C                                        Normalize RPARM's
                     DO 105 IP = 1,NRPARM
C                                        Re: WGT for merge will be 1.0
                        IF (ANOTA(IP).EQ.2) THEN
                           OPARM(IP) = VBUFF(KBUFF+IP) / WGT
                        ELSE
                           OPARM(IP) = VBUFF(KBUFF+IP)
                           END IF
 105                    CONTINUE
C                                        Baseline number. Not weighted
C                    OPARM(ILOCB+1) = VBUFF(KBUFF+ILOCB+1)
C                                        Source number. Not weighted
C                    IF (ILOCSU.GE.0) OPARM(ILOCSU+1) =
C    *                  VBUFF(KBUFF+ILOCSU+1)
C                                        FQ id. Not weighted
C                    IF (ILOCFQ.GE.0) OPARM(ILOCFQ+1) =
C    *                  VBUFF(KBUFF+ILOCFQ+1)
C                                        Integration time, not
C                                        weighted
C                    IF (ILOCIT.GT.0) OPARM(ILOCIT+1) =
C    *                  VBUFF(KBUFF+ILOCIT+1)
C                                        Copy new parms array to
C                                        output buffer.
                     CALL RCOPY (NRPARM, OPARM, BUFF2(IPTRO))
C                                       Compressed?
                     IF (ISCOMP) THEN
C                                        Normalize data. Correlator loop
                        DO 110 I = 1,MCOR
                           INDEX2 = (I-1)*3
                           INDEXV = KBUFF + NRPARM + (I-1)*3
                           IF (VBUFF(INDEXV+3).GT.0.0) THEN
                              DIV = 1.0 / VBUFF(INDEXV+3)
                              RESULT(INDEX2+1) = VBUFF(INDEXV+1) * DIV
                              RESULT(INDEX2+2) = VBUFF(INDEXV+2) * DIV
                              RESULT(INDEX2+3) = VBUFF(INDEXV+3)
C                                       store the result of IND = 1
                              IF (IND .EQ. 1) THEN
                                 VBUFF1(INDEXV+1) = VBUFF(INDEXV+1)*DIV
                                 VBUFF1(INDEXV+2) = VBUFF(INDEXV+2)*DIV
C                                       print out the full averaged
C                                       data for IF=1, channel=1
                                 IF (I .EQ. 1) THEN
                                    IF (ITPRT) THEN
                                       WRITE (MSGTXT,2800)
                                       CALL MSGWRT (6)
                                       WRITE (MSGTXT,2900)
                                       CALL MSGWRT (6)
                                       ITPRT = .FALSE.
                                       END IF
                                    IF (ILOCB.GE.0) THEN
                                       IIA1 = VBUFF(KBUFF+ILOCB+1)/256 +
     *                                    0.1
                                       IIA2 = VBUFF(KBUFF+ILOCB+1) -
     *                                    IIA1*256 + 0.1
                                    ELSE
                                       IIA1 = VBUFF(KBUFF+ILOCA1+1)+0.1
                                       IIA2 = VBUFF(KBUFF+ILOCA2+1)+0.1
                                       END IF
C                                    VRE = VBUFF(INDEXV+1)*DIV
C                                    VIM = VBUFF(INDEXV+2)*DIV
                                    VRE = VBUFF1(INDEXV+1)
                                    VIM = VBUFF1(INDEXV+2)
                                    WRITE (MSGTXT,3000) IIA1, IIA2, VRE,
     *                                 VIM, SQRT (VRE*VRE+VIM*VIM),
     *                                 ATAN2(VIM,VRE)*180.0/3.1415926
                                    CALL MSGWRT (6)
                                    END IF
                                 END IF
C
                           ELSE
C                                       No data
                              RESULT(INDEX2+1) = 0.0
                              RESULT(INDEX2+2) = 0.0
                              RESULT(INDEX2+3) = -1.0
                              END IF
 110                       CONTINUE
C                                       Pack/copy to output buffer
                        INDEX2 = IPTRO + NRPARM + (I-1)*3 - 1
                        CALL ZUVPAK (NCOPY, RESULT, BUFF2(IPTRO+ILOCWT),
     *                     BUFF2(IPTRO+NRPARM))
                     ELSE
C                                       Uncompressed data:
C                                       Normalize data. Correlator loop
                        DO 115 I = 1,MCOR
                           INDEX2 = IPTRO + NRPARM + (I-1)*3 - 1
                           INDEXV = KBUFF + NRPARM + (I-1)*3
                           IF (VBUFF(INDEXV+3).GT.0.0) THEN
                              DIV = 1.0 / VBUFF(INDEXV+3)
                              BUFF2(INDEX2+1) = VBUFF(INDEXV+1) * DIV
                              BUFF2(INDEX2+2) = VBUFF(INDEXV+2) * DIV
                              BUFF2(INDEX2+3) = VBUFF(INDEXV+3)
C                                       store the result of IND = 1
                              IF (IND .EQ. 1) THEN
                                 VBUFF1(INDEXV+1) = VBUFF(INDEXV+1)*DIV
                                 VBUFF1(INDEXV+2) = VBUFF(INDEXV+2)*DIV
C                                       print out the full averaged
C                                       data for IF=1, channel=1
                                 IF (I .EQ. 1) THEN
                                    IF (ITPRT) THEN
                                       WRITE (MSGTXT,2800)
                                       CALL MSGWRT (6)
                                       WRITE (MSGTXT,2900)
                                       CALL MSGWRT (6)
                                       ITPRT = .FALSE.
                                       END IF
                                    IF (ILOCB.GE.0) THEN
                                       IIA1 = VBUFF(KBUFF+ILOCB+1) / 256
     *                                    + 0.1
                                       IIA2 = VBUFF(KBUFF+ILOCB+1) -
     *                                    IIA1*256 + 0.1
                                    ELSE
                                       IIA1 = VBUFF(KBUFF+ILOCA1+1)+0.1
                                       IIA2 = VBUFF(KBUFF+ILOCA2+1)+0.1
                                       END IF
                                    VRE = VBUFF1(INDEXV+1)
                                    VIM = VBUFF1(INDEXV+2)
                                    WRITE (MSGTXT,3000) IIA1, IIA2, VRE,
     *                                 VIM, SQRT (VRE*VRE+VIM*VIM),
     *                                 ATAN2 (VIM,VRE)*180.0/3.1415926
                                    CALL MSGWRT (6)
                                    END IF
                                 END IF


                           ELSE
C                                       No data
                              BUFF2(INDEX2+1) = 0.0
                              BUFF2(INDEX2+2) = 0.0
                              BUFF2(INDEX2+3) = -1.0
                              END IF
 115                       CONTINUE
                        END IF
C                                        Update counters.
                     XCOUNT = XCOUNT + 1
C                                        Writing beyond EOF ?
                     IF (XCOUNT.GT.NVOUT) THEN
                        K = MAX (5000, NVOUT/10)
                        CALL UVSIZE (LRECO, K, IP)
                        CALL ZEXPND (LUNO, DISKO, ONAME, IP, IRET)
                        IF (IRET.EQ.0) THEN
                           NVOUT = NVOUT + K
                        ELSE
                           WRITE (MSGTXT,1060) IRET
                           CALL MSGWRT (8)
                           GO TO 999
                           END IF
                        END IF
C                                       skip writng output for IND=1
                     IF (IND .EQ. 1) GO TO 116
                     IPTRO = IPTRO + LRECO
                     NIOUT = NIOUT + 1
C                                        Write output
C                                        Only actually write when
C                                        the buffer is full.
                     IF (NIOUT.GE.NIOLIM) THEN
C                                        Use NIOUT instead of NIOLIM
C                                        so last record ok.
                        CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT,
     *                     KBIND, JRET)
                        IF (JRET.NE.0) THEN
                           WRITE (MSGTXT,1113) JRET
                           IRET = 5
                           GO TO 999
                           END IF
                        IPTRO = KBIND
                        NIOLIM = NIOUT
                        NIOUT = 0
                        END IF
  116                CONTINUE
C                                        Jump here if no data for bas.
  124             CONTINUE
C                                        End of baseline loop.
  126          CONTINUE
C                                        Jump here if output not
C                                        written
  128       CONTINUE
C                                        Close up after last record.
            IF (NUMVIS.EQ.-1) THEN
C                                       First write any unwritten
C                                       data.
C
C                                       skip writng output for IND=1
               IF (IND .EQ. 1) GO TO 129
               IF (NIOUT.GT.0) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT,
     *               KBIND, JRET)
                  IF (JRET.NE.0) THEN
                     WRITE (MSGTXT,1113) JRET
                     CALL MSGWRT (8)
                     IRET = 9
                     GO TO 999
                     END IF
                  END IF
C                                        Then flush buffers to output
               NIOFLS = 0
               CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOFLS, KBIND,
     *            JRET)
               IF (JRET.NE.0) THEN
                  WRITE (MSGTXT,1130) JRET
                  CALL MSGWRT (8)
                  IRET = 6
                  GO TO 999
                  END IF
  129          CONTINUE
C                                        Tell history
               YINC = YINC * 86400.0
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1135) YINC
C                                        Tell history about reduced
C                                        output.
               IF (XINC.GT.1.0) THEN
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1145) XINC
                  END IF
C                                        Write no. written to history.
               IF (IND .EQ. 2) THEN
                  NUMHIS = NUMHIS + 1
                  WRITE (HISCRD(NUMHIS),1150) XCOUNT
                  WRITE (MSGTXT,1150) XCOUNT
                  CALL MSGWRT (5)
                  END IF
C
                  GO TO 999
               END IF
C
C                                        First record, skip to here.
  180    CONTINUE
C                                        Set up for next integration
CRCW                                     Removed 0.95 here
         IDAY = CT
         TYINC =  YINC
         X8 = (CT-IDAY) / TYINC
         TLAST = IDAY + DINT (X8) * TYINC + TYINC
         TCHK = TLAST - 1.1 * TYINC
C                                        Reset accumulators, etc.
         I2TMP = LBAS * NFQID * (1+NRPARM+NMCOR)
         IF (I2TMP .GT. ACSIZE) I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
         BLCUR = IBAS
         LSTSOU = CURSOU
         LSTFQ = CURFQ
C                                        Reset FQ ID index
         NFQID = 0
         CALL FILL (64, 0, IFQTRA)
         END IF
C                                        Accumulate current datum.
C                                        Get baseline number for accum.
      KBAS = 1
      IF (LTB) KBAS = IA2*(IA2-1)/2 + IA1
C                                        Get FQ ID for accumulation.
      IF (CURFQ .GT. 64) THEN
         WRITE (MSGTXT,1170) CURFQ
         CALL MSGWRT (8)
         IRET = 11
         GO TO 999
         ENDIF
C                                        Update FQ ID index for
C                                        this accumulation.
      IF (CURFQ .GT. 0) THEN
         IF (IFQTRA(CURFQ) .EQ. 0) THEN
            NFQID = NFQID + 1
            IFQTRA(CURFQ) = NFQID
            ENDIF
         KFQID = IFQTRA(CURFQ)
      ELSE
         KFQID = 1
         NFQID = 1
      ENDIF
C                                        Get index of first element of
C                                        accumulation buffer for this
C                                        baseline.  That element will
C                                        accumulate the weight for PARMS
      KBUFF = (KBAS - 1 + (KFQID - 1) * LBAS) * (NRPARM + 1 + NMCOR) + 1
C                                       use the index of the VBUFF1
C                                       as it was stored
      KBUFF1 =(KBAS - 1 + (KFQID - 1) * LBAS) * (NRPAR1 + 1 + NMCOR) + 1
C                                        Check that accumulation
C                                        buffer is not exceeded.
      IF (KBUFF .GT. KLIM) THEN
         WRITE (MSGTXT,1180) KBUFF
         CALL MSGWRT (8)
         IRET = 12
         GO TO 999
         ENDIF
C                                        Initialize parms weight.
      WGT = 0.0
      DO 220 I = 1,MCOR
         INDEX = KBUFF + NRPARM + (I-1)*3
C                                        Check weight.
         IF (VIS(3,I).GT.0.0) THEN
C                                        Accumulate vis data.
C                                       Average
            IF (IND .EQ. 1) THEN
C                                       regular accumulation
               VBUFF(INDEX+1) = VBUFF(INDEX+1) +
     *            VIS(1,I)*VIS(3,I)
               VBUFF(INDEX+2) = VBUFF(INDEX+2) +
     *            VIS(2,I)*VIS(3,I)
               VBUFF(INDEX+3) = VBUFF(INDEX+3) + VIS(3,I)
            ELSE
               INDEX1 = KBUFF1 + NRPAR1 + (I-1)*3
C                                       subtruct the value calculated
C                                       at IND=1
               VBUFF(INDEX+1) = VBUFF(INDEX+1) +
     *                (VIS(1,I)-VBUFF1(INDEX1+1))*VIS(3,I)
C     *                (VIS(1,I)-VBUFF1(INDEX+1))*VIS(3,I)
               VBUFF(INDEX+2) = VBUFF(INDEX+2) +
     *            (VIS(2,I)-VBUFF1(INDEX1+2))*VIS(3,I)
C     *            (VIS(2,I)-VBUFF1(INDEX+2))*VIS(3,I)
               VBUFF(INDEX+3) = VBUFF(INDEX+3) + VIS(3,I)
               END IF
C                                         Use largest weight for uv etc
            WGT = MAX (WGT, VIS(3,I))
            END IF
 220     CONTINUE
C                                         Accumulate PARMS only if some
C                                         data was good.
      IF (WGT.GT.0.0) THEN
C                                        For merge.
         DO 240 IP = 1,NRPARM
            IF (ANOTA(IP).EQ.2) THEN
               VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)*WGT
            ELSE IF (ANOTA(IP).EQ.1) THEN
               VBUFF(KBUFF+IP) = VBUFF(KBUFF+IP) + RPARM(IP)
            ELSE
               IF (VBUFF(KBUFF+IP).EQ.0.0) VBUFF(KBUFF+IP) =
     *            RPARM(IP)
               END IF
  240       CONTINUE
C                                        Accumulate weights.
         VBUFF(KBUFF) = VBUFF(KBUFF) + WGT
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('UVAVDO: ANTENNA NUMBERS NOT IN ',
     *      'INCREASING ORDER IN BASL NO')
 1060 FORMAT ('UVAVDO: ERROR',I6,' EXPANDING OUTPUT FILE')
 1090 FORMAT ('UVAVDO: RECORDS NOT IN TIME (TB) ORDER')
 1104 FORMAT ('UVAVDO: SOMETHING WRONG WITH SORT')
 1113 FORMAT ('UVAVDO: ERROR',I3,' WRITING VIS FILE')
 1130 FORMAT ('UVAVDO: ERROR',I3,' CLOSING VIS FILE')
 1135 FORMAT ('UVAVDO: Average time=',F9.2,' sec.')
 1145 FORMAT ('UVAVDO: only one in ',F6.0,' output records actually',
     *        ' written.')
 1150 FORMAT ('UVAVDO: ',I10,' Visibility records written')
 1160 FORMAT ('UVAVDO: Parameter MXINDX too small (',I5,' )')
 1170 FORMAT ('UVAVDO: Increase IFQTRA dimension to at least',I5)
 1180 FORMAT ('UVAVDO: Parameter ACSIZE too small (',I8,' )')
 2800 FORMAT (10X, 'Parameters of averaged VISs used for subtraction')
 2900 FORMAT ('  IF=1, CHAN=1|  ', 'Basel  ', '   Re   ',
     *        '     Im   ','    Amp   ', '    Phas  ')
 3000 FORMAT (16X, I2, '-', I2, 2X, F8.4, 2X,F8.4, 2X,F8.4,4X, F6.1)
      END

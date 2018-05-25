LOCAL INCLUDE 'BLAVG.INC'
C                                       Local include for BLAVG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER  CATIN(256), CATOUT(256), SEQIN, SEQOUT, DISKIN, DISKO,
     *   BCHANN, ECHANN, BIFF, EIFF, TNF, TNIF, TNS, INCFB, INCIFB,
     *   NUMHIS, JBUFSZ, ILOCWT, NRPARO, OANT1, OANT2, ANTIDX(256),
     *   STKRL, STKLR
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XSOUR(4,30)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(10)*64
      REAL   XSIN, XDISIN, XSOUT, XDISO, XFQID, XBCHAN, XECHAN, XBIF,
     *   XEIF, XTIME(8), XANTS(50), XFLAG, XDOCAL, XGUSE, XSOLIN,
     *   XREF, BUFF1(UVBFSS), BUFF2(UVBFSS), DTUTC, XSUBA
      LOGICAL   ISCOMP
      INTEGER ACSIZE
C                                       ACSIZE = size of accum. array
      PARAMETER (ACSIZE = 2500000)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /HDRS/ CATIN, CATOUT
      COMMON /INPTS/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XTIME, XFQID,
     *   XBCHAN, XECHAN, XBIF, XEIF, XANTS, XFLAG, XSUBA, XDOCAL, XGUSE,
     *   XNAMOU, XCLAOU, XSOUT, XDISO, XSOLIN, XREF, SEQIN, SEQOUT,
     *   DISKIN, DISKO, BCHANN, ECHANN, BIFF, EIFF, TNF, TNIF, TNS,
     *   INCFB, INCIFB, NUMHIS, ILOCWT, NRPARO, ISCOMP, ANTIDX, DTUTC,
     *   OANT1, OANT2, STKRL, STKLR
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
LOCAL END
      PROGRAM BLAVG
C-----------------------------------------------------------------------
C! Average cross-polarized uv data over baselines.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2004, 2006-2009, 2015
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
C   The primary function of UVAVG is to average data which it does
C   if any OPCODE except 'MERG' is used.
C
C   UVAVG averages a uv data set to a maximum time given by YINC.
C   Data can be in either 'TB' or 'BT' order.  The accumulation buffer
C   has a maximum size that will allow 8 correlators on 30 stations
C   or 128 spectral channels on 7 stations for the 'TB' sort order with
C   10 parameters in the PARMS array.  The data is averaged weighted by
C   the data weights.  The PARMS are averaged weighted by the largest
C   weight in the data record except the baseline number, for which
C   the last one is passed.
C
C   For OPCODE='MERG', UVAVG performs the merge function previously
C   by VBMRG.  The time specified by YINC is taken to be the average
C   time of the input data.  In each input time on each baseline,
C   only one data point is selected and passed.  This is useful for
C   VLB data processed with multiple passes so that there are multiple
C   copies of some data.  It is also useful for putting together
C   polarization data sets when different polarizations were processed
C   separately.
C
C   If XINC if greater than 1, only every XINC'th group of output
C   records (eg all baselines for an average interval) will be written.
C   This should be useful in creating small data sets that still have
C   about the uv coverage of a large data set and still has points
C   appropriately alligned for self cal.  Might be used, for example
C   in the first iterations of hybrid mapping.
C
C   Based on program UVAVG
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of input UV data.
C   INCLASS        CLAIN         Class of input UV data.
C   INSEQ          SEQIN         Seq. of input UV data.
C   INDISK         DISKIN        Disk number of input UV data.
C   OUTNAME        NAMOUT        Name of the output uv file.
C   OUTCLASS       CLAOUT        Class of the output uv file.
C   OUTSEQ         SEQOUT        Seq. number of output uv data.
C   OUTDISK        DISKO         Disk number of the output file.
C   YINC           YINC          Integration time (sec) min=0.2
C   XINC           XINC          Write only XINC'th output recs.
C   OPCODE         OPCODE        Average or merge.
C   BLAVG Programmer: K. J. Leppanen  Apr. 1994.
C   UVAVG Programmer R. C. Walker, Feb. 1984.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGM*6
      INCLUDE 'BLAVG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA PRGM /'BLAVG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL BLAVIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Average data
      CALL BLAVUV (IRET)
C                                       Update history and copy tables
      IF (IRET.EQ.0) CALL BLAVHI
C                                       close down
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE BLAVIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   BLAVIN gets input parameters for BLAVG and creates an output file
C   if necessary.
C   Inputs:  PRGM   C*6      Task name
C   Output:  JERR   I        Error code: quit if > 0.
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGM*6, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   LUNAN, ANVER, IROUND, JERR, NPARM, IERR, OLDCNO,
     *   I, J, FLAG, REFANT
      LOGICAL   T, F, SELECT, EXCLUD
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'BLAVG.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA T, F /.TRUE.,.FALSE./
      DATA LUNAN /27/
C-----------------------------------------------------------------------
C                                       Initialize I/O
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
C
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 203
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
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      IF (XSOLIN .LT. 0.001) XSOLIN = 1.0
      XSOLIN = XSOLIN / 1440.0
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C                                       Put data selection criteria
C                                       into correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 1.0/86400.0
      FRQSEL = IROUND(XFQID)
      IF (FRQSEL .LT. 1) FRQSEL = 1
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 20      CONTINUE
      SUBARR = IROUND (XSUBA)
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      FGVER = IROUND (XFLAG)
      CLUSE = IROUND (XGUSE)
      REFANT = IROUND (XREF)
C                                       Antenna selection
C                                       ANTIDX(I) = 0 -> ant. I excluded
      EXCLUD = F
      SELECT = F
      DO 22 I = 1,256
         ANTIDX(I) = 0
 22      CONTINUE
      FLAG = 1
      DO 24 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
         IF (IABS (ANTENS(I)) .GT. 256) THEN
            WRITE (MSGTXT,1020)
            JERR = 5
            GO TO 990
            END IF
         IF (ANTENS(I) .LT. 0) EXCLUD = T
         IF (ANTENS(I) .NE. 0) THEN
            ANTIDX(IABS(ANTENS(I))) = FLAG
            SELECT = T
            END IF
 24      CONTINUE
      IF (EXCLUD .OR. .NOT.SELECT) FLAG = 0
      J = 1
      DO 26 I = 1,256
         IF (ANTIDX(I) .EQ. FLAG) THEN
            ANTIDX(I) = J
            J = J + 1
         ELSE
            ANTIDX(I) = 0
            END IF
 26      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
C                                       Error finding file.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
C                                       Error copying CATBLK.
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Copy old CATBLK
      CALL COPY (256, CATBLK, CATIN)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Number of Stokes, IF, channels.
      TNS = 1
      IF (JLOCS.GT.0) TNS = CATBLK(KINAX+JLOCS)
      TNIF = 1
      IF (JLOCIF.GT.0) TNIF = CATBLK(KINAX+JLOCIF)
      TNF = 1
      IF (JLOCF.GT.0) TNF = CATBLK(KINAX+JLOCF)
C                                       Channels for phase alignment
      BCHANN = IROUND (XBCHAN)
      BCHANN = MAX (1, MIN (BCHANN, TNF))
      ECHANN = IROUND (XECHAN)
      IF (ECHANN.LT.BCHANN) ECHANN = TNF
      ECHANN = MAX (1, MIN (ECHANN, TNF))
C                                       IFs to use for phase alignm.
C                                       BIFF < 0 => don't use that IF
      BIFF = IROUND (XBIF)
      EIFF = IROUND (XEIF)
      IF (BIFF.LT.0) THEN
         WRITE (MSGTXT,1045) IABS (BIFF)
         CALL MSGWRT(2)
      ELSE
         BIFF = MAX (1, MIN (BIFF, TNIF))
         IF (EIFF.LT.BIFF) EIFF = TNIF
         EIFF = MAX (1, MIN (EIFF, TNIF))
         END IF
C                                       Check if RL and LR present.
      IF (ICOR0-TNS+1 .NE. -4) THEN
         WRITE (MSGTXT,1050)
         JERR = 5
         GO TO 990
         END IF
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
C                                       Check if sort order 'TB'
      IF (ISORT.NE.'TB') THEN
         WRITE (MSGTXT,1051) ISORT
         JERR = 5
         GO TO 990
         END IF
C                                       Get AN table
      ANVER = 1
      CALL ANTINI ('READ', BUFF1, DISKIN, OLDCNO, ANVER, CATIN, LUNAN,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, NUMORB, XYZHAN, TFRAME,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1052) IERR
         JERR = 5
         GO TO 990
         END IF
C                                       Antenna numbers for output
      IANRNO = 1
      CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME, STAXYZ,
     *   ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *   POLCA, POLTYB, POLAB, POLCB, IERR)
      OANT1 = NOSTA
      IANRNO = 2
      CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME, STAXYZ,
     *   ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA, POLAA,
     *   POLCA, POLTYB, POLAB, POLCB, IERR)
      OANT2 = NOSTA
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1052) IERR
         JERR = 5
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, JERR)
      IF (REFANT.GT.0) THEN
         IF (REFANT.GT.OANT1) THEN
C                                       RL->RR, LR->LL
            OANT2 = REFANT
            STKRL = -1
            STKLR = -2
            END IF
         IF (REFANT.LE.OANT1) THEN
C                                       RL->LL, LR->RR
            OANT1 = REFANT
            STKRL = -2
            STKLR = -1
            END IF
      ELSE
         WRITE (MSGTXT,1053)
         JERR = 5
         GO TO 990
         END IF
C                                       Message about output baseline
      WRITE (MSGTXT,1054) OANT1,OANT2
      CALL MSGWRT(2)
C                                       Data time - UTC
      DTUTC = DATUTC / 86400.0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Output file will have only
C                                       RL&LR disguised as RR&LL
      CATBLK(KINAX+JLOCS) = 2
      CATD(KDCRV+JLOCS) = -1.0D0
      CATR(KRCIC+JLOCS) = -1.0
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1055) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      SEQOUT = CATBLK(KIIMS)
C                                        Copy new CATBLK
      CALL COPY (256, CATBLK, CATOUT)
      JERR = 0
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLAVIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1020 FORMAT ('BLAVIN: SPECIFIED ANTENNA NUMBERS OUT OF RANGE')
 1030 FORMAT ('BLAVIN: ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,
     *   ' DISK=',I3,' USID=',I5)
 1040 FORMAT ('BLAVIN: ERROR',I3,' COPYING CATBLK ')
 1045 FORMAT ('Not using IF',I3,' for phase alignment')
 1050 FORMAT ('BLAVIN: STOKES RL OR LR MISSING')
 1051 FORMAT ('BLAVIN: SORT ORDER ',A2,' NOT TB AS REQUIRED')
 1052 FORMAT ('BLAVIN: ERROR',I3,' READING ANTENNA TABLE')
 1053 FORMAT ('BLAVIN: REFERENCE ANTENNA MUST BE SPECIFIED')
 1054 FORMAT ('OUTPUT BASELINE ',I3,' -',I3)
 1055 FORMAT ('BLAVIN: ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE BLAVUV (IRET)
C-----------------------------------------------------------------------
C   BLAVUV sends uv data one point at a time to the average/merge
C   routine and then writes the modified data if requested.
C   Input in common:
C      ISCOMP  L  If true data is compressed
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER PHNAME(6)*48
      INTEGER   IRET, IPTRO, LUNO, LRECO, INDO, NIOLIM, NIOUT, KBIND,
     *   IA1, IA2, ILENBU, VO, BO, NUMVIS, XCOUNT
      LOGICAL   T, F
      INCLUDE 'BLAVG.INC'
      REAL      BASEL, RPARMS(10), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA VO, BO /0, 1/
      DATA LUNO /17/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       LRECO = length of output rec.
      LRECO = (LREC - NRPARM) * 2 / TNS + NRPARM
C                                       NRPARO = # RPARMS in output
      NRPARO = NRPARM
C                                       Open input file
      CALL UVGET ('INIT', DUMMY, DUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Update parameters for buffers
C                                       having only two stokes
      LREC = (LREC - NRPARM) * 2 / TNS + NRPARM
      INCFB = INCF
      INCIFB = INCIF
      IF (JLOCF.GT.JLOCS) INCFB = INCF * 2 / TNS
      IF (JLOCIF.GT.JLOCS) INCIFB = INCIF * 2 / TNS
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
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
C                                       Init counters.
      NUMVIS = 0
C                                       Loop
C                                       Read vis. record.
 100     CALL UVGET ('READ', RPARMS, BUFF1, IRET)
C                                        Test for end of data
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) GO TO 999
C                                        Prepare to call BLAVDO
            IF (ILOCB.GE.0) THEN
               BASEL = RPARMS(ILOCB+1)
               IA1 = BASEL / 256. + 0.1
               IA2 = BASEL - IA1*256. + 0.1
            ELSE
               IA1 = RPARMS(ILOCA1+1) + 0.1
               IA2 = RPARMS(ILOCA2+1) + 0.1
               END IF
C                                        No zero baselines
            IF (IA1.EQ.IA2) GO TO 190
C                                        Use antenna index numbers
            IA1 = ANTIDX(IA1)
            IA2 = ANTIDX(IA2)
C                                        Baseline wanted?
            IF (IA1.EQ.0 .OR. IA2.EQ.0) GO TO 190
C                                        Be sure antenna numbers are
C                                        in the expected order.
            IF (IA1 .GE. IA2) THEN
               WRITE (MSGTXT,1110)
               IRET = 7
               GO TO 990
               END IF
            NUMVIS = NUMVIS + 1
            CALL BLAVDO (NUMVIS, RPARMS(ILOCT+1), BUFF1, RPARMS,
     *         IA1, IA2, IRET, LUNO, INDO, NIOLIM, LRECO, IPTRO,
     *         NIOUT, XCOUNT)
C                                       Error (fatal)
            IF (IRET.LE.0) GO TO 190
               WRITE (MSGTXT,1120) IRET
               GO TO 990
 190        CONTINUE
         GO TO 100
C                                       Final call to BLAVDO.
 200  NUMVIS = -1
      CALL BLAVDO (NUMVIS, RPARMS(ILOCT+1), BUFF1, RPARMS, IA1, IA2,
     *   IRET, LUNO, INDO, NIOLIM, LRECO, IPTRO, NIOUT, XCOUNT)
      IF (IRET.LE.0) GO TO 250
         WRITE (MSGTXT,1120) IRET
         GO TO 990
C                                       Recover output CATBLK
 250  CALL COPY (256, CATOUT, CATBLK)
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOSE', DUMMY, DUMMY, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BLAVUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('BLAVUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1110 FORMAT ('BLAVUV: ANTENNA NUMBERS NOT IN INCREASING ORDER IN BASL',
     *  ' NO')
 1120 FORMAT ('BLAVUV: BLAVDO ERROR',I3)
      END
      SUBROUTINE BLAVHI
C-----------------------------------------------------------------------
C   BLAVHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(16)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I
      INTEGER   NONOT
      LOGICAL   T
      INCLUDE 'BLAVG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2, T /27,28, .TRUE./
      DATA NONOT /16/
      DATA NOTTYP /'NX','SN','CL','AT','CT','FG','BP','VT','IM','MC',
     *   'TY', 'PC', 'HF', 'GC', 'CQ', 'BL'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM,NAMOUT,CLAOUT,SEQOUT,DISKO,LUN2,
     *   BUFF2,IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Add selection/calibration
C                                       criteria:
C                                       Sources
      WRITE (HILINE,3000) TSKNAM
      IF (NSOUWD.LE.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (NSOUWD.LE.0) GO TO 25
C                                       Included or excluded?
      WRITE (HILINE,3001) TSKNAM
      IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       1st 2 and label.
      WRITE (HILINE,3003) TSKNAM, SOURCS(1), SOURCS(2)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      IF (NSOUWD.LE.2) GO TO 25
C                                       Rest of sources
      DO 20 I = 3,NSOUWD,2
         WRITE (HILINE,3004) TSKNAM, SOURCS(I), SOURCS(I+1)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
 20      CONTINUE
 25   CONTINUE
C                                       Timerange
      CALL HITIME (TSTART, TEND, LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Flag table
      WRITE (HILINE,3014) TSKNAM, FGVER
      IF (DOFLAG) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Calibration info
C                                       Gain tables
      IF (DOCAL) WRITE (HILINE,3019) TSKNAM, CLUSE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 100 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 100     CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 300
         WRITE (MSGTXT,1019)
         CALL MSGWRT (6)
C                                        Update CATBLK.
 300   CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BLAVHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1019 FORMAT ('BLAVHI: ERROR COPYING TABLES')
 3000 FORMAT (A6,' SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,' /Sources excluded:')
 3002 FORMAT (A6,' /Sources included:')
 3003 FORMAT (A6,' SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'          ,''',A16,''',''',A16,'''')
 3014 FORMAT (A6,' FLAGVER =',I3,' /Flagging table used')
 3019 FORMAT (A6,' GAINUSE = ',I3,' /CL table applied')
      END
      SUBROUTINE BLAVDO (NUMVIS, T, VIS, RPARM, IA1, IA2,
     *   IRET, LUNO, INDO, NIOLIM, LRECO, IPTRO, NIOUT, XCOUNT)
C-----------------------------------------------------------------------
C   BLAVDO averages cross-hand visibilities in time and over baselines.
C   Inputs:
C      NUMVIS     I    Visibility number, -1=> final call, no data
C                      passed but allows any operations to be completed.
C                      Data sent back will be wirtten to output file.
C      T          R    Time in days since 0 IAT on the first day for
C                      which there is data.
C      RPARM(*)   I    Random parameter array which includes U,V,W etc
C                      but also any other random parameters.
C      VIS(3,*)   R    Vis data in order real, imaginary, weight (Jy)
C      IA1        I    First antenna number
C      IA2        I    Second antenna number (IA2 must be > IA1)
C      LUNO       I    Unit number for output.
C      INDO       I    For output, from ZOPEN
C      NIOLIM     I    Maximum number or records per buffer.
C      IPTRO      I    Output pointer - initialize before first call.
C      NIOUT      I    Number of records in buffer.  Init before first
C                                 call
C      XCOUNT     I    Output record counter.  Init before first call.
C   Inputs from COMMON
C      NRPARM     I    # random parameters.
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
C-----------------------------------------------------------------------
C                                        Declares for call.
      INTEGER   IA1, IA2, NUMVIS, IRET, LUNO, INDO, NIOLIM, IPTRO,
     *   NIOUT, LRECO, XCOUNT
      REAL      T, VIS(*), RPARM(10)
C                                       Main declares.
      INTEGER   KREC, KLIM, I2TMP, REFIF, JRET, VISIDX, BUFIDX, AVGIDX,
     *   OUTIDX, IDAY, NIOFLS, STK, I, ICHAN, IFNO, ICOR, KBIND, MCOR,
     *   NMCOR, KBUFF, BLCUR, NCOPY, CURSRC, LSTSRC, CURFQ, LBAS, IBAS,
     *   KBAS
      REAL      DIV, WGT, AVGWGT, TLAST, CT, TCHK, OPARM(10)
      DOUBLE PRECISION X8
      INCLUDE 'BLAVG.INC'
      REAL      AVGBUF(UVBFSS), UNITYV(UVBFSS)
      REAL      VBUFF(ACSIZE)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      SAVE VBUFF, AVGBUF, UNITYV, NCOPY, TLAST, MCOR, NMCOR,
     *   KLIM, OPARM, REFIF
C-----------------------------------------------------------------------
      IRET = -1
C                                        Initial call
      IF (NUMVIS.EQ.1) THEN
         NUMHIS = 0
C                                        Number of visibilities in
C                                        output file.
         NCOPY = LRECO - NRPARO
C                                        Check PARM array.
C                                        Check for unknown parameters.
         IF ((.NOT.ISCOMP .AND. (NRPARM.GT.8)) .OR.
     *         (ISCOMP .AND. (NRPARM.GT.10))) THEN
            WRITE (MSGTXT,1040) NRPARM
            CALL MSGWRT (8)
            NUMHIS = NUMHIS + 1
            WRITE (HISCRD(NUMHIS),1040)
            END IF
C                                        Initialize OPARM to 1.0
         CALL RFILL (NRPARM, 1.0, OPARM)
C                                        Set constant output RPARM's
C                                        Baseline to OANT1-OANT2
         IF (ILOCB.GE.0) THEN
            OPARM(ILOCB+1) = 256 * OANT1 + OANT2
         ELSE
            OPARM(ILOCA1+1) = OANT1
            OPARM(ILOCA2+1) = OANT2
            END IF
C                                        u,v,w to zero
         OPARM(ILOCU+1) = 0.0
         OPARM(ILOCV+1) = 0.0
         OPARM(ILOCW+1) = 0.0
C                                        Integration time
         IF (ILOCIT.GT.0) OPARM(ILOCIT+1) = XSOLIN*86400.0
C                                        Set counters etc.
         XCOUNT = 0
         TLAST = -1.0
         TCHK = -1.0
         BLCUR = -1
         MCOR = (LREC - NRPARM) / CATBLK(KINAX)
         NMCOR = MCOR * 3
         CURSRC = -1
         IF (ILOCSU.GE.0) CURSRC = RPARM(1+ILOCSU) + 0.5
         LSTSRC = CURSRC
         CURFQ = 1
         IF (ILOCFQ.GE.0) CURFQ = RPARM(1+ILOCFQ) + 0.5
C                                        Normalize phase for this IF #
         REFIF = BIFF
         IF (BIFF.LT.1) REFIF = 1
         IF (BIFF.EQ.-1) REFIF = 2
C                                        Limit to accumulation buffer.
         KLIM = ACSIZE - (1 + NRPARM + NMCOR)
C                                        Set unity visibility buffer
         DO 80 I = 1,MCOR
            VISIDX = (I-1) * 3
            UNITYV(VISIDX+1) = 1.0
            UNITYV(VISIDX+2) = 0.0
            UNITYV(VISIDX+3) = 1.0
 80         CONTINUE
C                                        Reset accumulation buffer
         I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
         END IF
C                                        End of special first record
C                                        processing.
C
C                                        If in final call, skip tests.
      IF (NUMVIS.NE.-1) THEN
C                                        Get UT from data time
         CT = T - DTUTC
C                                        Get baseline.
         IF (ILOCB.GE.0) THEN
            IBAS = RPARM(ILOCB+1) + 0.1
         ELSE
            IBAS = RPARM(1+ILOCA1) + 0.1
            IBAS = 32768 * IBAS + RPARM(1+ILOCA2) + 0.1
            END IF
C                                        Be sure of sort order.
         IF (CT.LT.TCHK .AND. (IBAS.EQ.BLCUR)) THEN
            WRITE (MSGTXT,1090)
            CALL MSGWRT (8)
            IRET = 4
            GO TO 999
            END IF
         END IF
C                                        Source Number, FQ id
      CURSRC = -1
      IF (ILOCSU.GE.0) CURSRC = RPARM(1+ILOCSU) + 0.5
      CURFQ = 1
      IF (ILOCFQ.GE.0) CURFQ = RPARM(1+ILOCFQ) + 0.5
      IF (CURFQ .NE. FRQSEL) THEN
         WRITE (MSGTXT,1095)
         CALL MSGWRT (8)
         IRET = 4
         GO TO 999
         END IF
C                                        Check if output needed.
      IF ((CT.GT.TLAST) .OR. (NUMVIS.EQ.-1) .OR.
     *   (CURSRC.NE.LSTSRC)) THEN
C                                        No output for first record.
         IF (NUMVIS.EQ.1) GO TO 180
C                                        Average baselines
            DO 120 KREC = 1,LBAS
               KBUFF = (KREC-1) * (1+NRPARM+NMCOR) + 1
               IF (KBUFF .GT. KLIM) GO TO 120
C                                        Weight for RPARM.
C                                        Will be 0.0 if no valid data
               WGT = VBUFF(KBUFF)
               IF (WGT.GT.0.0) THEN
C                                        Align phases.
                  IF (AVGWGT.GT.0.0) CALL ROTATE (VBUFF(KBUFF+NRPARM+1),
     *               AVGBUF(NRPARM+1), BIFF, EIFF, BCHANN, ECHANN)
C                                        Average visibilities
                  AVGIDX = NRPARM
                  DO 110 I = 1,MCOR
                     BUFIDX = KBUFF + AVGIDX
                     AVGBUF(AVGIDX+1) = AVGBUF(AVGIDX+1) +
     *                  VBUFF(BUFIDX+1)
                     AVGBUF(AVGIDX+2) = AVGBUF(AVGIDX+2) +
     *                  VBUFF(BUFIDX+2)
                     AVGBUF(AVGIDX+3) = AVGBUF(AVGIDX+3) +
     *                  VBUFF(BUFIDX+3)
                     AVGIDX = AVGIDX + 3
 110                 CONTINUE
C                                        Average time
                  AVGBUF(ILOCT+1) = AVGBUF(ILOCT+1) +
     *               VBUFF(KBUFF+ILOCT+1)
C                                        Source number
                  IF (ILOCSU.GE.0) AVGBUF(ILOCSU+1) =
     *               VBUFF(KBUFF+ILOCSU+1)
C                                        Sum RPARM weight
                  AVGWGT = AVGWGT + WGT
                  END IF
 120           CONTINUE
C                                        No data - no output
            IF (AVGWGT.GT.0.0) THEN
C                                        Set output RPARM
C                                        Time (weighted)
               OPARM(ILOCT+1) = AVGBUF(ILOCT+1) / AVGWGT
C                                        Source number
               IF (ILOCSU.GE.0) OPARM(ILOCSU+1) =
     *            AVGBUF(ILOCSU+1)
C                                        Freq ID
               IF (ILOCFQ.GE.0) OPARM(ILOCFQ+1) = FRQSEL
C                                        Copy new parms array to
C                                        output buffer.
               CALL RCOPY (NRPARM, OPARM, BUFF2(IPTRO))
C                                        Rotate phases so as to set
C                                        first IF to appr. zero
               CALL ROTATE (AVGBUF(NRPARM+1), UNITYV, REFIF, REFIF,
     *            BCHANN, ECHANN)
C                                        Compressed?
               IF (ISCOMP) THEN
C                                        Normalize data
                  AVGIDX = NRPARM
                  DO 130 I = 1,MCOR
                     IF (AVGBUF(AVGIDX+3).GT.0.0) THEN
                        DIV = 1.0 / AVGBUF(AVGIDX+3)
                        AVGBUF(AVGIDX+1) = AVGBUF(AVGIDX+1) * DIV
                        AVGBUF(AVGIDX+2) = AVGBUF(AVGIDX+2) * DIV
                     ELSE
C                                       No data
                        AVGBUF(AVGIDX+1) = 0.0
                        AVGBUF(AVGIDX+2) = 0.0
                        AVGBUF(AVGIDX+3) = -1.0
                        END IF
                     AVGIDX = AVGIDX + 3
 130                 CONTINUE
C                                       Pack/copy to output buffer
                  CALL ZUVPAK (NCOPY, AVGBUF(NRPARM+1),
     *               BUFF2(IPTRO+ILOCWT), BUFF2(IPTRO+NRPARO))
               ELSE
C                                       Uncompressed data:
C                                       Normalize
                  AVGIDX = NRPARM
                  DO 140 I = 1,MCOR
                     OUTIDX = IPTRO + AVGIDX - 1
                     IF (AVGBUF(AVGIDX+3).GT.0.0) THEN
                        DIV = 1.0 / AVGBUF(AVGIDX+3)
                        BUFF2(OUTIDX+1) = AVGBUF(AVGIDX+1) * DIV
                        BUFF2(OUTIDX+2) = AVGBUF(AVGIDX+2) * DIV
                        BUFF2(OUTIDX+3) = AVGBUF(AVGIDX+3)
                     ELSE
C                                       No data
                        BUFF2(OUTIDX+1) = 0.0
                        BUFF2(OUTIDX+2) = 0.0
                        BUFF2(OUTIDX+3) = -1.0
                        END IF
                     AVGIDX = AVGIDX + 3
 140                 CONTINUE
                  END IF
C                                        Update counters.
               XCOUNT = XCOUNT + 1
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                        Write output
C                                        Only actually write when
C                                        the buffer is full.
               IF (NIOUT.GE.NIOLIM) THEN
C                                        Use NIOUT instead of NIOLIM
C                                        so last record ok.
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOUT,
     *               KBIND, JRET)
                  IF (JRET.NE.0) THEN
                     WRITE (MSGTXT,1113) JRET
                     IRET = 5
                     GO TO 999
                     END IF
                  IPTRO = KBIND
                  NIOLIM = NIOUT
                  NIOUT = 0
                  END IF
               END IF
C                                        Close up after last record.
            IF (NUMVIS.EQ.-1) THEN
C                                        First write any unwritten
C                                        data.
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
C                                        Write average time to history
               XSOLIN = XSOLIN * 1440.0
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1140) XSOLIN
C                                        Write no. written to history.
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1150) XCOUNT
               WRITE (MSGTXT,1150) XCOUNT
               CALL MSGWRT (5)
               GO TO 999
               END IF
C
C                                        First record, skip to here.
  180       CONTINUE
C                                        Set up for next integration
         IDAY = CT
         X8 = (CT-IDAY) / XSOLIN
         TLAST = IDAY + DINT (X8) * XSOLIN + XSOLIN
         TCHK = TLAST - 1.1 * XSOLIN
C                                        Reset accumulators, etc.
         I2TMP = LBAS * (1+NRPARM+NMCOR)
         IF (I2TMP .GT. ACSIZE) I2TMP = ACSIZE
         CALL RFILL (I2TMP, 0.0, VBUFF)
         AVGWGT = 0.0
         CALL RFILL (UVBFSS, 0.0, AVGBUF)
         BLCUR = IBAS
         LSTSRC = CURSRC
         LBAS = 0
         END IF
C                                        End of write stuff
C
C                                        Accumulate current datum.
C                                        Get baseline number for accum.
      KBAS = (IA2-1)*(IA2-2)/2 + IA1
C                                        Update the largest baseline #
      IF (KBAS .GT. LBAS) LBAS = KBAS
C                                        Get index of first element of
C                                        accumulation buffer for this
C                                        baseline.  That element will
C                                        accumulate the weight for PARMS
      KBUFF = (KBAS - 1) * (NRPARM + 1 + NMCOR) + 1
C                                        Check that accumulation
C                                        buffer is not exceeded.
      IF (KBUFF .GT. KLIM) THEN
         WRITE (MSGTXT,1180) KBUFF
         CALL MSGWRT (8)
         IRET = 12
         GO TO 999
         END IF
C                                        Initialize parms weight.
      WGT = 0.0
C                                        Loop over stokes RL and LR
      STK = STKRL
      DO 360 ICOR = -3,-4,-1
         DO 340 IFNO = 1,TNIF
            VISIDX = ABS (ICOR-ICOR0)*INCS + (IFNO-1)*INCIF
            BUFIDX = KBUFF + NRPARM + (-1-STK)*INCS + (IFNO-1)*INCIFB
            DO 320 ICHAN = 1,TNF
C                                        Check weight.
               IF (VIS(VISIDX+3).GT.0.0) THEN
C                                        Accumulate crosspol. data.
C                                        Average
                  VBUFF(BUFIDX+1) =
     *               VBUFF(BUFIDX+1) + VIS(VISIDX+1) * VIS(VISIDX+3)
                  VBUFF(BUFIDX+2) =
     *               VBUFF(BUFIDX+2) + VIS(VISIDX+2) * VIS(VISIDX+3)
                  VBUFF(BUFIDX+3) = VBUFF(BUFIDX+3) + VIS(VISIDX+3)
C                                         Use largest weight for time
                  WGT = MAX (WGT, VIS(VISIDX+3))
                  END IF
               VISIDX = VISIDX + INCF
               BUFIDX = BUFIDX + INCFB
 320           CONTINUE
 340        CONTINUE
         STK = STKLR
 360     CONTINUE
C                                         Accumulate RPARMS only if
C                                         some data was good.
      IF (WGT.GT.0.0) THEN
         VBUFF(KBUFF+ILOCT+1) =
     *      VBUFF(KBUFF+ILOCT+1) + RPARM(ILOCT+1)*WGT
C                                        Accumulate weights.
         VBUFF(KBUFF) = VBUFF(KBUFF) + WGT
C                                        Don't weight source id.
         IF (ILOCSU.GE.0) VBUFF(KBUFF+ILOCSU+1) = RPARM(ILOCSU+1)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('BLAVDO: NO. OF PARAMETERS ',I3,
     *   ' NOT NORMAL - AVG MAY BE BAD')
 1090 FORMAT ('BLAVDO: RECORDS NOT IN TIME (TB) ORDER')
 1095 FORMAT ('BLAVDO: ILLEGAL FREQUENCY ID')
 1113 FORMAT ('BLAVDO: ERROR',I3,' WRITING VIS FILE')
 1130 FORMAT ('BLAVDO: ERROR',I3,' CLOSING VIS FILE')
 1140 FORMAT ('BLAVDO: Average time=',F7.2,' min.')
 1150 FORMAT ('BLAVDO: ',I10,' Visibility records written')
 1180 FORMAT ('BLAVDO: Parameter ACSIZE too small (',I8,' )')
      END
      SUBROUTINE ROTATE (VIS, VISREF, BIFN, EIFN, BCHN, ECHN)
C-----------------------------------------------------------------------
C   ROTATE rotates the phases of the input visibilities to match the
C   phases of the reference visibilities. The value of rotation is
C   determined separately for RL and LR from all channels specified
C   by BIFN, EIFN, BCHN and ECHN, and then applied to all cross-hand
C   channels in VIS. It is assumed that the pointers in COMMON /UVHDR/
C   are valid for VIS.
C   Inputs:
C      VIS(*)     R    Visibility data record
C      VISREF(*)  R    Reference visibility record
C      BIFN       I    First IF for phase comparison. If < 0, all the
C                      IFs except this are included (EIFN is ignored).
C      EIFN       I    Last IF for phase comparison
C      BCHN       I    First channel for phase comparison
C      ECHN       I    Last channel for phase comparison
C   Inputs from COMMON
C      TNF        I    # channels in VIS
C      TNIF       I    # IF in VIS
C      TNS        I    # Stokes in VIS
C   Output:
C      VIS        R    Visibility data record rotated in phase
C   Programmer: K. J. Leppanen, Apr. 1994.
C-----------------------------------------------------------------------
      REAL     VIS(*), VISREF(*)
      INTEGER  BIFN, EIFN, BCHN, ECHN
      INTEGER  ICOR, IFNO, ICHAN, INDEX, BADIF
      COMPLEX  CVIS, CREF, SUM, PHASOR
C
      INCLUDE 'BLAVG.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
C                                       See if an IF should be excluded
      BADIF = 0
      IF (BIFN .LT. 1) THEN
         BADIF = IABS (BIFN)
         BIFN = 1
         EIFN = TNIF
         END IF
C                                       Stokes RL and LR in the buffers
      DO 50 ICOR = 0,1
C                                       Find the rotation phasor
C                                       by channelwise division
         SUM = CMPLX (0.0,0.0)
         DO 20 IFNO = BIFN,EIFN
            IF (IFNO .EQ. BADIF) GO TO 20
            INDEX = ICOR*INCS + (IFNO-1)*INCIFB + (BCHN-1)*INCFB
            DO 10 ICHAN = BCHN,ECHN
               IF ((VIS(INDEX+3).GT.0.0) .AND. (VISREF(INDEX+3).GT.0.0))
     *            THEN
                  CVIS = CMPLX (VIS(INDEX+1),VIS(INDEX+2))
                  IF (CABS(CVIS).GT.1.0E-8) THEN
                     CREF = CMPLX (VISREF(INDEX+1),VISREF(INDEX+2))
                     SUM = SUM + CVIS / CABS (CVIS) / CREF * CABS (CREF)
     *                  * VIS(INDEX+3) * VISREF(INDEX+3)
                     END IF
                  END IF
               INDEX = INDEX + INCFB
 10            CONTINUE
 20         CONTINUE
C                                       Rotate the data
C                                       (all channels and IFs)
         IF (CABS(SUM).GE.1.0E-8) THEN
            PHASOR = CABS (SUM) / SUM
            DO 40 IFNO = 1,TNIF
               INDEX = ICOR*INCS + (IFNO-1)*INCIFB
               DO 30 ICHAN = 1,TNF
                  CVIS = CMPLX (VIS(INDEX+1),VIS(INDEX+2)) * PHASOR
                  VIS(INDEX+1) = REAL (CVIS)
                  VIS(INDEX+2) = AIMAG (CVIS)
                  INDEX = INDEX + INCFB
 30               CONTINUE
 40            CONTINUE
            END IF
 50      CONTINUE
C
 999  RETURN
      END

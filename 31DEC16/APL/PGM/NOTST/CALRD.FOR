LOCAL INCLUDE 'CALRD.INC'
C                                       Local include for CALRD
      INCLUDE 'INCS:PMAD.INC'
      INCLUDE 'INCS:DCAT.INC'
      HOLLERITH XOBJ(2), XBAND
      REAL      XDISK
      COMMON /INPARM/ XOBJ, XBAND, XDISK
C
      CHARACTER NAME*12, CLASS*6, INFILE*48, HDRBUF*2880
      INTEGER   FDVEC(50)
      COMMON /CHRCOM/ HDRBUF, NAME, CLASS, INFILE
C
      INTEGER   IBLNK
      INTEGER   TBIND, NTAPE, CNO, NBPIX, TAPEBP, TABLES,
     *   INBUFF(MABFSS), TAPBUF(29184), UNKNWN
      LOGICAL   ISBLNK, STDEXT
      REAL      PCMATX(7,7), CDMATX(7,7), PVMATX(7,7)
      DOUBLE PRECISION POS11(2), SCALE, OFFSET, ISCALE, IZERO
C
      COMMON /MLTAP/ SCALE, OFFSET, ISCALE, IZERO, POS11, TAPBUF,
     *   INBUFF, IBLNK, ISBLNK, STDEXT, UNKNWN,
     *   FDVEC, TBIND, NTAPE, CNO, NBPIX, TAPEBP, TABLES, PCMATX,
     *   CDMATX, PVMATX
C                                                          End CALRD
LOCAL END
      PROGRAM CALRD
C-----------------------------------------------------------------------
C! Reads images of calibrators from FITS files distributed with aips
C# TASK FITS CALIBRATION
C-----------------------------------------------------------------------
C;  Copyright (C) 2004, 2010-2012, 2015
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
C   CALRD is the main control program for conversion of the IBM-360
C   DEC FORMAT map or a FITS FORMAT map into the standard map format.
C   AIPS input parameters:
C      OBJECT    C*8   Source name
C      BAND      C*4   Array/band code
C      OUTDISK   I     Output disk unit #
C-----------------------------------------------------------------------
      INTEGER   IRET, HLUN, HBUFF(256), ISCR(256), IERR, NPARM
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA HLUN, NPARM /27, 4/
C-----------------------------------------------------------------------
C                                       get parameters open input
      CALL CALRDI (NPARM, ISCR, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       process FITS file
      CALL FITTAP (HLUN, HBUFF, IRET)
      IF (IRET.NE.0) GO TO 950
C                                       Close output
 950  CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
      IF (IRET.EQ.0) IRET = IERR
C                                       Clean up
 995  CALL DIE (IRET, HBUFF)
C
 999  STOP
      END
      SUBROUTINE CALRDI (NPARM, ISCR, IRET)
C-----------------------------------------------------------------------
C   CALRDI initializes CALRD
C   Outputs:
C      KVOL   I        Output disk #
C      ISCR   I(256)   Scratch buffer.
C      IRET   I        Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      INTEGER   NPARM, ISCR(256), IRET
C
      CHARACTER PRGNAM*6, OBJECT*8, BAND*4
      INTEGER   IERR, ITRIM, I
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGNAM /'CALRD '/
C-----------------------------------------------------------------------
C                                       Initialize disk character.
      CALL ZDCHIN (.TRUE.)
      CALL HIINIT (3)
      CALL VHDRIN
      NCFILE = 0
      NSCR = 0
C                                       Initialize for AIPS
      IRET = 0
      CALL GTPARM (PRGNAM, NPARM, RQUICK, XOBJ, ISCR, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         RQUICK = .FALSE.
         IRET = 16
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       name parameters
      CALL H2CHR (8, 1, XOBJ, OBJECT)
      CALL H2CHR (4, 1, XBAND, BAND)
      I = ITRIM (OBJECT)
      NAME = OBJECT(:I) // '_' // BAND
      CLASS = 'MODEL'
C                                       Check if disk file output
      IF (RQUICK) CALL RELPOP (IRET, ISCR, IERR)
C                                       set up I/O
      CALL FILL (50, 0, FDVEC)
C                                       Buffer size.
      FDVEC(3) = (29184 * NBITWD) / 8
C                                       Logical record size (FITS)
      FDVEC(2) = 2880
C                                       Disk output.
      FDVEC(1) = 25
      FDVEC(5) = 1
C                                       Try first choice name
      I = ITRIM (NAME)
      INFILE = 'AIPSTARS:' // NAME(:I) // '.MODEL'
      CALL CHR2H (48, INFILE, 1, FDVEC(7))
      MSGSUP = 32000
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.EQ.0) THEN
         CALL TAPIO ('CLOS', FDVEC, TAPBUF, TBIND, IERR)
C                                       Try other choice name
      ELSE
         INFILE = 'FITS:' // NAME(:I) // '.MODEL'
         CALL CHR2H (48, INFILE, 1, FDVEC(7))
         END IF
      MSGSUP = 0
C                                       open FITS disk file
      CALL TAPIO ('OPRD', FDVEC, TAPBUF, TBIND, IRET)
      IF (IRET.EQ.0) THEN
         MSGTXT = 'Reading disk file ' // INFILE
         CALL MSGWRT (2)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR IN GTPARM.  IER=',I7)
      END
      SUBROUTINE FITTAP (HLUN, HBUFF, IERR)
C-----------------------------------------------------------------------
C  Process FITS type tape header, data, and extension files.
C  Inputs:
C     HLUN   I        Open History file LUN.
C     HBUFF  I(256)   History file work I/O buffer.
C  Outputs:
C     IERR   I        Error code, 0=ok.
C-----------------------------------------------------------------------
      INTEGER   HLUN, HBUFF(256), IERR
C
      INTEGER   ISLOT, SEQ, KVOL, NUMTAB
      LOGICAL   EOF, MORTAB
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      KVOL = XDISK + .5
      SEQ = 0
C                                       Does header & history using a
C                                       temporary name in catalog.
      CALL FITHDR (KVOL, HLUN, HBUFF, ISLOT, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get the data and store in file
      CALL FITDAT (KVOL, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       aff inputs to history
      CALL HISINP (KVOL, HLUN, HBUFF)
C                                       Standard fits extension records.
      CALL FITRXM (KVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
      IF (IERR.GT.0) GO TO 999
      MORTAB = IERR.LT.0
      IERR = 0
C                                       Old tables records.
      IF (.NOT.EOF) CALL MLTABL (KVOL, HLUN, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (UNKNWN.GT.0) THEN
         WRITE (MSGTXT,1030) UNKNWN
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('WARNING:',I7,' unknown records skipped while reading',
     *   ' tape')
      END
      SUBROUTINE FITDAT (KVOL, IER)
C-----------------------------------------------------------------------
C   FITDAT reads the input data file and scales the data to disk.
C   Inputs:
C      KVOL  I     desired map disk
C   Outputs:
C      IER   I     Error return:  0--> okay
C                                 1--> error condition
C-----------------------------------------------------------------------
      INTEGER   KVOL, IER
C
      CHARACTER MNAME*48
      INTEGER   BLKS, IERR, IWIN(4), NBKOF1, IOFF, NX, NY, IDEPTH(5),
     *   NBYB, I, INX, INY, IBL, ITEMP, NXY, I3, I3B, I4, I4B, I5, I5B,
     *   I6, I6B, I7, I7B, DLUN, DIND,  NTAPVL, III, L0, L1, L2, NXX,
     *   OUTIND
      INCLUDE 'CALRD.INC'
      REAL      BUFF(MABFSS), MMAX, MMIN, INBUFR(MABFSS)
      DOUBLE PRECISION    BSC, BZE, DPBUFR(MABFSS/2), DTEMP
      LOGICAL   T, WASBLK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (INBUFR(1), INBUFF(1), DPBUFR(1))
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      DLUN = 16
      MMAX = -1.E20
      MMIN =  -MMAX
      WASBLK = .FALSE.
C                                       Open map file.
      CALL ZPHFIL ('MA', KVOL, CNO, 1, MNAME, IERR)
      CALL ZOPEN (DLUN, DIND, KVOL, MNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) MNAME, IERR
         GO TO 980
         END IF
C                                       Initialize
      IER = 0
      BSC = SCALE
      IF (BSC.EQ.0.0D0) BSC = 1.0D0
      BZE = OFFSET
      NBYB = MABFSS * 2
C                                       Set window parms
      I3B = MAX (1, CATBLK(KINAX+2))
      I4B = MAX (1, CATBLK(KINAX+3))
      I5B = MAX (1, CATBLK(KINAX+4))
      I6B = MAX (1, CATBLK(KINAX+5))
      I7B = MAX (1, CATBLK(KINAX+6))
      IWIN(1) = 1
      IWIN(2) = 1
      IWIN(3) = CATBLK(KINAX)
      IWIN(4) = CATBLK(KINAX+1)
      NY = IWIN(4)
      NX = IWIN(3)
      INX = CATBLK(KINAX)
      INY = CATBLK(KINAX+1)
C                                       Initialize tape
      NBPIX = TAPEBP
      BLKS = (ABS(NBPIX) / 8)
      NTAPVL = 2880 / BLKS
      IOFF = NTAPVL
      BLKS = BLKS * NX * NY * I3B
      BLKS = BLKS * I4B * I5B * I6B * I7B
      BLKS = (BLKS - 1) / 2880 + 1
      BLKS = BLKS - 1
      IF (IERR.NE.0) GO TO 970
C                                       Test for Kitt Peak "error"
      IF ((IBLNK.EQ.0) .AND. (NBPIX.EQ.8)) ISBLNK = .FALSE.
      DO 200 I7 = 1,I7B
      DO 200 I6 = 1,I6B
      DO 200 I5 = 1,I5B
      DO 200 I4 = 1,I4B
      DO 200 I3 = 1,I3B
C                                       Initialize disk
         IDEPTH(1) = I3
         IDEPTH(2) = I4
         IDEPTH(3) = I5
         IDEPTH(4) = I6
         IDEPTH(5) = I7
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IDEPTH, NBKOF1,
     *      IERR)
         IF (IERR.NE.0) GO TO 990
         NBKOF1 = NBKOF1 + 1
         CALL MINIT ('WRIT', DLUN, DIND, INX, INY, IWIN, BUFF, NBYB,
     *      NBKOF1, IERR)
         IF (IERR.EQ.0) GO TO 30
            WRITE (MSGTXT,1020) IERR
            GO TO 980
C                                       Begin read/write loop
 30      DO 195 I = 1,NY
C                                       Write a map line
            CALL MDISK ('WRIT', DLUN, DIND, BUFF, OUTIND, IERR)
            IF (IERR.EQ.0) GO TO 40
               WRITE (MSGTXT,1030) IERR, I
               GO TO 980
 40         NXY = NX
            IBL = 0
C                                       Copy and read until entire map
C                                       row filled.
 55         NXX = MIN (NXY, NTAPVL-IOFF)
C                                       Need more tape values.
               IF (NXX.GT.0) GO TO 60
                  BLKS = BLKS - 1
                  CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
                  IOFF = 0
                  IF (NBPIX.EQ.8) CALL ZI8IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.16) CALL ZI16IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.32) CALL ZI32IL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-32) CALL ZR32RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (NBPIX.EQ.-64) CALL ZR64RL (NTAPVL, 1,
     *               TAPBUF(TBIND), INBUFF)
                  IF (IERR.EQ.0) GO TO 55
                     GO TO 970
C                                       INT in: copy convert max/min
 60            IF ((NBPIX.EQ.8) .OR. (NBPIX.EQ.16) .OR. (NBPIX.EQ.32))
     *            THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  IF (ISBLNK) THEN
                     DO 100 III = 1,NXX
                        L1 = L2 + III
                        ITEMP = INBUFF(L0+III)
C                                       Blank pixel found
                        IF (ITEMP.EQ.IBLNK) THEN
                           BUFF(L1) = FBLANK
                           WASBLK = .TRUE.
C                                       scale
                        ELSE
                           BUFF(L1) = BSC * ITEMP + BZE
                           MMIN = MIN (MMIN, BUFF(L1))
                           MMAX = MAX (MMAX, BUFF(L1))
                           END IF
 100                    CONTINUE
                  ELSE
                     DO 115 III = 1,NXX
                        L1 = L2 + III
                        BUFF(L1) = BSC * INBUFF(L0+III) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
 115                    CONTINUE
                     END IF
                  GO TO 190
C                                       IEEE 64-bit in
               ELSE IF (NBPIX.EQ.-64) THEN
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 150 III = 1,NXX
                     L1 = L2 + III
                     DTEMP = DPBUFR(L0+III)
                     IF (DTEMP.EQ.DBLANK) THEN
                        WASBLK = .TRUE.
                        BUFF(L1) = FBLANK
                     ELSE
                        BUFF(L1) = BSC * DTEMP + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 150                 CONTINUE
C                                       IEEE 32-bit in
               ELSE
                  L0 = IOFF
                  L2 = OUTIND + IBL - 1
                  DO 160 III = 1,NXX
                     L1 = L2 + III
                     BUFF(L1) = INBUFR(L0+III)
                     IF (BUFF(L1).EQ.FBLANK) THEN
                        WASBLK = .TRUE.
                     ELSE
                        BUFF(L1) = BSC * BUFF(L1) + BZE
                        MMIN = MIN (MMIN, BUFF(L1))
                        MMAX = MAX (MMAX, BUFF(L1))
                        END IF
 160                 CONTINUE
                  END IF
C                                       Up the counters
 190           IBL = IBL + NXX
               IOFF = IOFF + NXX
               NXY = NXY - NXX
C                                       loop back if needed to finish
               IF (NXY.GT.0) GO TO 55
 195         CONTINUE
C                                       Flush this plane.
         CALL MDISK ('FINI', DLUN, DIND, BUFF, OUTIND, IERR)
         IF (IERR.NE.0) GO TO 970
 200     CONTINUE
C                                       close files
      CATR(KRDMX) = MMAX
      CATR(KRDMN) = MMIN
      CATR(KRBLK) = 0.0
      IF (WASBLK) CATR(KRBLK) = FBLANK
      CALL MAPCLS ('WRIT', KVOL, CNO, DLUN, DIND, CATBLK, T, BUFF, IERR)
      NCFILE = NCFILE - 1
      GO TO 999
C                                       Error
 970  WRITE (MSGTXT,1970) IERR
 980  CALL MSGWRT (8)
      IF (IERR.EQ.4) THEN
         WRITE (MSGTXT,1980)
         CALL MSGWRT(8)
         END IF
 990  IER = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FITDAT: COULD NOT OPEN MAP ',6A4,' IER=',I4)
 1020 FORMAT ('FITDAT: COULD NOT INITIALIZE DISK FILE.  IER=',I4)
 1030 FORMAT ('FITDAT: COULD NOT WRITE DISK RECORD.  IER=',I3,
     *   ' LINE=',I4)
 1970 FORMAT ('FITDAT: COULD NOT READ INPUT.  IER=',I4)
 1980 FORMAT ('FITDAT: - MAYBE PREMATURE END OF FILE?  CHECK FILE SIZE')
      END
      SUBROUTINE HISINP (KVOL, HLUN, HBUFF)
C-----------------------------------------------------------------------
C   Add inputs to the history file.
C   Inputs:
C     KVOL   I         Disk volume number of history file and map.
C     HLUN   I         History file LUN.
C     HBUFF  I(256)    History I/O work buffer.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   KVOL, HLUN, HBUFF(256), IERR
      LOGICAL   T
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, FVOL(1), FCNO(1), HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1000) NAME, CLASS
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1001) CATBLK(KIIMS), KVOL
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1002) INFILE
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
      WRITE (HILINE,1003) RLSNAM
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
      IF (IERR.EQ.0) GO TO 999
C                                       Error.
 980  MSGTXT = 'WARNING: ERROR IN ADDING INPUTS TO HISTORY FILE'
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALRD OUTNAME =''',A12,'''',6X,'OUTCLASS =''',A6,
     *   '''')
 1001 FORMAT ('CALRD OUTSEQ =',I5,5X,5X,'OUTDISK=',I2)
 1002 FORMAT ('CALRD INFILE = ''',A,'''')
 1003 FORMAT ('CALRD RELEASE = ''',A7,'''')
      END
      SUBROUTINE FITHDR (KVOL, HLUN, HBUFF, ISLOT, IERR)
C-----------------------------------------------------------------------
C   FITHDR reads the tape which must be open and positioned at begin.
C   of file) and builds a catalog header and pointers from the
C   tape header records.  After the required fits cards are read a
C   map file with a temporary name is created and the history records
C   are recognized and written to the history file as the other header
C   cards are processed.  The file is later renamed to the correct name.
C   Inputs:
C     KVOL   I     Disk volume for cataloged map.
C     HLUN   I     History file logical unit number.
C     HBUFF  I(256)   work buffer.
C   Output:C
C     ISLOT  I     Catalog slot number for new map file.
C     NODATA L     True if tape contains no data section, else false.
C     ERR    I     =0 => ok
C                   other => quit
C-----------------------------------------------------------------------
      CHARACTER LINE*80, HILINE*72, CHTEMP*8
      INTEGER   HBUFF(256), ICARD, IE, IST, IERR, ISLOT, IREC, I,
     *   IN, IS, IAX, HLUN, ICEND, KVOL
      LOGICAL   END, T, ISHIST, NODATA, DOHI
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Initialize BLANK values flag
C                                       to false.
      NODATA = .FALSE.
      ISBLNK = .FALSE.
      CALL CATCLR (CATBLK)
C                                       Initialize header values.
      CALL CATINI (CATBLK)
      SCALE = 1.0D0
      OFFSET = 0.0D0
      ISCALE = 1.0D0
      IZERO = 0.0D0
      CALL RFILL (49, 0.0, PCMATX)
      CALL RFILL (49, 0.0, PVMATX)
      CALL RFILL (49, 0.0, CDMATX)
C                                       Record 1 read
      CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) THEN
         WRITE (1000,MSGTXT) IERR
         GO TO 990
         END IF
      CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       Decode required cards.
      CALL IMREQC (HDRBUF, ICEND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       More defaults.
      DO 10 I = 1,KICTPN
         CATR(KRCRP+I-1) = CATBLK(KINAX+I-1) / 2
         CATR(KRCIC+I-1) = 1.0
 10      CONTINUE
C                                       Create map with temporary name.
C                                       Map will be renamed later.
      CALL CHR2H (12, NAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = 0
      CATBLK(KIIMU) = NLUSER
      CALL MCREAT (KVOL, CNO, HBUFF, IERR)
      ISLOT = CNO
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
      NCFILE = 1
      FCNO(NCFILE) = CNO
      FRW(NCFILE) = 2
      FVOL(NCFILE) = KVOL
C                                       Create HI file
      CALL HICREA (HLUN, KVOL, CNO, CATBLK, HBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Header msg in HI
      LINE ='--------------------------------------------------'//
     *   '------------------'
      CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
      LINE = '/Begin "HISTORY" information found in fits tape ' //
     *   'header by CALRD'
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       See if need to parse rest of
C                                       header.
C
      ICARD = ICEND + 1
C                                       Loop until END card found.
      DO 90 IREC = 1,100000000
C                                       Read next record.
         IF (ICARD.GT.36) THEN
            CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, IERR)
            IF (IERR.NE.0) GO TO 999
            CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
            ICARD = 1
            END IF
C                                       Parse card, put value in hdr.
         CALL IMPARS (ICARD, HDRBUF, ISHIST, END, IERR)
         IF (END) GO TO 100
C                                       Add to history file.
         IF (IERR.GT.0) THEN
            IST = 80 * ICARD - 79
            CHTEMP = HDRBUF(IST:)
            DOHI = (CHTEMP.EQ.'HISTORY') .OR. (CHTEMP.EQ.'COMMENT')
     *         .OR. (CHTEMP.EQ.' ')
            LINE = HDRBUF(IST:)
            IST = 1
            IF (DOHI) IST = IST + 8
            CALL HIAD80 (HLUN, IST, LINE, HBUFF, IERR)
         ELSE IF (IERR.EQ.-1) THEN
            IS = (ICARD - 1) * 80 + 1
            IE = IS + 79
            CALL PUTCRD (HDRBUF(IS:IE), KVOL, CNO, IERR)
            IF (IERR.GT.1) THEN
               WRITE (MSGTXT,1080) IERR
               CALL MSGWRT (7)
               GO TO 999
               END IF
            END IF
         ICARD = ICARD + 1
 90      CONTINUE
C                                       Read more cards than we expected
      WRITE (MSGTXT,1090)
      GO TO 990
C                                       End card found.
 100  CONTINUE
C                                       Make axis increments non zero
C                                       to help out dumb programs.
      IN = KINAX
      IS = KRCIC
      IE = IS + CATBLK(KIDIM) - 1
      DO 200 IAX = IS,IE
         IF ((CATR(IAX).EQ.0.0).AND.(CATBLK(IN).EQ.1)) CATR(IAX) = 1.0
         IN = IN + 1
 200     CONTINUE
C                                       End FITS header section in HI
      WRITE (HILINE,1110)
      CALL HIADD (HLUN, HILINE, HBUFF, IERR)
      LINE ='--------------------------------------------------'//
     *   '------------------'
      IF (IERR.EQ.0) CALL HIAD80 (HLUN, 1, LINE, HBUFF, IERR)
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C                                       PC -> CROTA
C                                       CD -> CDELT, CROTA
      CALL PCHDR (PCMATX, CDMATX, PVMATX)
      GO TO 999
C
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('MAP CREATE ERROR',I4)
 1020 FORMAT ('HISTORY CREATE ERROR',I4)
 1080 FORMAT ('ERROR',I5,' ADDING KEYWORD TO HEADER FILE')
 1090 FORMAT ('READ MORE THAN 10**8 CARDS WITHOUT FINDING AN END CARD')
 1110 FORMAT ('/END FITS tape header "HISTORY" information')
      END
      SUBROUTINE IMREQC (FITBLK, ICARD, IERR)
C-----------------------------------------------------------------------
C   This routine will look for the required cards in a FIT header block
C   SIMPLE, BITPIX, NAXIS, NAXISn, and update a catalog header with the
C   information from these cards.
C   Inputs:
C      FITBLK  C*2880   a block of fit header data.
C   Outputs:
C      ICARD   I        The number of the last card parsed.
C      IERR    I        0=ok, 1=messed up. An error message will
C                                     be printed.
C   COMMON /MAPHDR/ Axis dimension information will be filled in.
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, EXTEND*8, KL*80
      INTEGER   NPNT, ITYP, NAXIS, ITABNO, IVAL, IKEYWD, I, IAX
      LOGICAL   ISHIST, END
      DOUBLE PRECISION X
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFUV.INC'
      INCLUDE 'INCS:VFUV.INC'
      DATA EXTEND /'EXTEND  '/
C-----------------------------------------------------------------------
C                                       Look for SIMPLE=T card
      I = NCT + NKT
      ICARD = 1
      IKEYWD = 1
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GETLG (KL, 80, NPNT, ITYP)
C                                       Not .TRUE.
      IF (ITYP.NE.1) GO TO 940
C                                       Look for BITPIX.
      ICARD = ICARD + 1
      IKEYWD = IKEYWD + 1
      NPNT = 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Check value of BITPIX
      CALL GETNUM (KL, 80, NPNT, X)
      IF (X.EQ.DBLANK) GO TO 975
      IF (X.GE.0.) IVAL = X + 0.1
      IF (X.LT.0.) IVAL = X - 0.1
      TAPEBP = IVAL
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
C
      IAX = KINAX
      CATBLK(KIDIM) = NAXIS
C                                       Check for invalid no. of axis
C                                       for our header.
      IF (NAXIS.GT.7) GO TO 960
C     Check NAXISm
      DO 30 I = 1,NAXIS
         ICARD = ICARD + 1
         IKEYWD = IKEYWD + 1
         NPNT = 1
         CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
         IF (IERR.NE.0) GO TO 999
         CALL GETCRD (ICARD, 1, 1, CWORD(IKEYWD), FITBLK, NPNT,
     *      KL, SYMBOL, ITABNO, ISHIST, END, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL GETNUM (KL, 80, NPNT, X)
         IF (X.EQ.DBLANK) GO TO 975
         CATBLK(IAX) = X + .01
         IAX = IAX + 1
 30      CONTINUE
C                                       Look for EXTEND = T card.
      IF (CATBLK(KINAX).EQ.0) GO TO 930
      ICARD = ICARD + 1
      CALL SKPBLK (FITBLK, ICARD, FDVEC, TAPBUF, TBIND, IERR)
      IF (IERR.NE.0) GO TO 999
      NPNT = 1
      CALL GETCRD (ICARD, 1, 1, EXTEND, FITBLK, NPNT, KL,
     *   SYMBOL, ITABNO, ISHIST, END, ITYP)
C                                       No extensions
      IF (END.OR.ISHIST.OR.(ITYP.NE.0)) THEN
         ICARD = ICARD - 1
         STDEXT = .FALSE.
      ELSE
         CALL GETLG (KL, 80, NPNT, ITYP)
         IF (ITYP.EQ.1) THEN
            STDEXT = .TRUE.
         ELSE
            ICARD = ICARD - 1
            STDEXT = .FALSE.
            END IF
         END IF
      GO TO 999
C                                       Probably a UV tape.
 930  WRITE (MSGTXT,1930)
      CALL MSGWRT (8)
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
 975  MSGTXT = 'IMREQC: VALUE ERROR PARSING ' // SYMBOL
C                                       Print error message set flag.
 980  CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1930 FORMAT ('THIS IS PROBABLY A UV DATA FILE THAT MUST BE READ WITH',
     *   ' UVLOD')
 1940 FORMAT ('NOT SIMPLE FITS TAPE. PROGRAM STOPPING.')
 1950 FORMAT ('INVALID BITS PER PIXEL =',I6)
 1960 FORMAT ('INVALID NUMBER OF AXIS =',I6)
 1970 FORMAT ('EXPECTED KEYWORD ',A8,'. FOUND ',A8,'.')
      END
      SUBROUTINE IMPARS (ICARD, FITBLK, ISHIST, END, IERR)
C-----------------------------------------------------------------------
C   IMPARS (parse FITS card) will unpack and interpret a card image
C   from a block of FITS data and put that data into the internal AIPS
C   header.
C   Inputs:
C      ICARD   I         The card number (1-36) in block to interpret.
C      FITBLK  C*2880    A block of FITS header data.
C   Outputs:
C      ISHIST  L         True iff a history card
C      END     L         True if end card found, else false.
C      IERR    I         error code 0=ok. 1=error, -1 => special header
C   COMMON /MAPHDR/
C   COMMON /FITINF/
C-----------------------------------------------------------------------
      INTEGER   ICARD, IERR
      LOGICAL   ISHIST, END
      CHARACTER FITBLK*2880
C
      CHARACTER SYMBOL*8, STR*68, KL*80
      DOUBLE PRECISION X
      REAL      VAL
      LOGICAL   LHIST, FIRST
      INTEGER   KPNTR(58), PNTR, IPOFF, TABNO, NPNT, KT, IL, IVAL,
     *   NCHAR, NBYT, NN, NNSTR, NPNTS
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIT.INC'
      EQUIVALENCE (KPNTR(1), KHOBJ)
      INCLUDE 'INCS:VFIT.INC'
C-----------------------------------------------------------------------
C                                       Find next symbol on the card
C                                       and look for it in the table.
      NPNT = 1
      NN = NKT + NCT
      NNSTR = NCT + 1
C                                       Loop for all possible values
C                                       on an AIPS HISTORY card.
      FIRST = .TRUE.
 10   CONTINUE
      CALL GETCRD (ICARD, NN, NNSTR, AWORD, FITBLK, NPNT, KL, SYMBOL,
     *    TABNO, LHIST, END, IERR)
      IF (END) GO TO 999
      IF ((IERR.EQ.1) .AND. ((SYMBOL(:2).EQ.'CD') .OR.
     *   (SYMBOL(:2).EQ.'PC') .OR. (SYMBOL(:2).EQ.'PV')))
     *   CALL PCCARD (0, KL, PCMATX, CDMATX, PVMATX)
      IF (IERR.NE.0) GO TO 999
      IF (FIRST) ISHIST = LHIST
      FIRST = .FALSE.
C                                       Header pointer for this
C                                       keyword, number bytes and
C                                       offset position from pointer
      PNTR = MOD (APOINT(TABNO), 1000)
      IPOFF = PNTR / 100
      PNTR = MOD (PNTR, 100)
      IF (PNTR.GT.0) PNTR = KPNTR(PNTR)
      NBYT = APOINT(TABNO) / 1000
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
            MSGTXT = 'LOGICAL VARIABLE HAS ILLEGAL VALUE: ' // SYMBOL
            GO TO 990
            END IF
C                                       Handle normal logical cases.
         CATBLK(PNTR+IPOFF) = IL
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
         IF (X.EQ.DBLANK) GO TO 975
C                                       Check for number special cases.
C                                       Blank pixel value.
         IF (AWORD(TABNO).EQ.'BLANK') THEN
            IF (X.EQ.-2147483648.0D0) THEN
               IBLNK = -2147483647 - 1
            ELSE
               IBLNK = X
               END IF
            ISBLNK = .TRUE.
C                                       Handle normal cases. Put value
C                                       into proper header slot.
C                                       2-byte integer
         ELSE IF (NBYT.EQ.2) THEN
            IVAL = X + SIGN (0.5D0, X)
            IF (PNTR.GT.0) THEN
               CATBLK(PNTR+IPOFF) = IVAL
            ELSE
               IF (AWORD(TABNO).EQ.'BITPIX') TAPEBP = IVAL
               IF (AWORD(TABNO).EQ.'TABLES') TABLES = IVAL
               END IF
C                                       4-byte real
         ELSE IF (NBYT.EQ.4) THEN
            IF ((AWORD(TABNO).EQ.'HISTORY') .AND. (X.GT.1.0E30))
     *         X = 1.0E30
            VAL = X
            IF (PNTR.GT.0) CATR(PNTR+IPOFF) = VAL
C                                       8-byte real
         ELSE IF (NBYT.EQ.8) THEN
            IF (PNTR.GT.0) THEN
               CATD(PNTR+IPOFF) = X
            ELSE
               IF (AWORD(TABNO).EQ.'BSCALE') SCALE = X
               IF (AWORD(TABNO).EQ.'ISCALE') ISCALE = X
               IF (AWORD(TABNO).EQ.'BZERO') OFFSET = X
               IF (AWORD(TABNO).EQ.'IZERO') IZERO = X
               END IF
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
C                                       Start string on integer boundary
C                                       IMCLASS
         IF (AWORD(TABNO).EQ.'IMCLASS') THEN
            IPOFF = NBYT * IPOFF + 1
            CALL CHFILL (NBYT, HBLANK, IPOFF, CATH(PNTR))
            CALL CHR2H (NCHAR, STR, IPOFF, CATH(PNTR))
C                                       Start string on real boundary.
         ELSE
            IPOFF = (NBYT / 4) * IPOFF
            CALL CHFILL (NBYT, HBLANK, 1, CATH(PNTR+IPOFF))
            CALL CHR2H (NCHAR, STR, 1, CATH(PNTR+IPOFF))
            END IF
         END IF
C                                       If this is a history card, look
C                                       for more values.
      IF (ISHIST) GO TO 10
      GO TO 999
C
 975  MSGTXT = 'IMPARS: NUMBER VALUE ERROR ON ' // SYMBOL
C                                       Error message
 990  CALL MSGWRT (7)
      IERR = 1
C
 999  RETURN
      END
      SUBROUTINE MLTABL (VOL, HLUN, HBUFF, IRET)
C-----------------------------------------------------------------------
C   MLTABL processes records following the normal FITS image.  If
C   TABLES <= 0, it simply counts the number of such records.  Else,
C   it parses through the Table records creating the appropriate
C   extension files and adding the table header cards to the
C   history file.
C   Inputs:  VOL    I         Output disk volume #
C            HLUN   I         LUN of open history file
C   In/Out:  HBUFF  I(256)    HI work buffer
C   Output:  IRET   I         Error code: 0 => ok, 8 => some error
C-----------------------------------------------------------------------
      INTEGER   VOL, HLUN, HBUFF(256), IRET
C
      CHARACTER ISTR*80, SYM*8, CARD*80, CTYPES(2)*4, LLCHAR*4,
     *   SYMS(15)*8, CHTM12*12, TABNAM*8, TTYPE(10)*8
      INTEGER   TABCNT, IRNO, IERR, TERR, TABVER, TABWID, TABCRD, NC,
     *   NPNT, ITYP, NSYMS, NCHAR, ITAB, NTYPES, IT, LUN, INC,
     *   BUFFER(768), IP, I, IREC, J, NDIM, JJ, IST
      LOGICAL   HISERR, EQUAL, NODATA, T, ISHIS
      REAL      RDATA(10), XINC
      DOUBLE PRECISION    X
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA NSYMS, NTYPES, LUN /15, 2, 28/
      DATA CTYPES,     LLCHAR
     *   /' CCC','  CC','LL  '/
      DATA SYMS /'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ',
     *   'TTYPE5  ', 'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ',
     *   'TTYPE9  ', 'TTYPE10 ', 'TABNAME ', 'TABVER  ',
     *   'TABCOUNT', 'TABWIDTH', 'TABCARDS'/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      CALL HIOPEN (HLUN, FVOL(1), FCNO(1), HBUFF, IERR)
      IERR = 0
      TERR = 0
      IRET = 0
      IF (TABLES.LE.0) GO TO 900
      HISERR = .FALSE.
      IRET = 8
C                                       Loop over tables
      NC = 0
      DO 200 ITAB = 1,TABLES
         WRITE (CARD,1000) ITAB
         IF (.NOT.HISERR) CALL HIAD80 (HLUN, 1, CARD, HBUFF, IERR)
         IF (IERR.NE.0) HISERR = .TRUE.
C                                       Init table parm values
         TABVER = 0
         TABCNT = 0
         TABWID = 0
         TABCRD = 0
         TABNAM = '        '
         DO 20 I = 1,10
            TTYPE(I) = '        '
 20         CONTINUE
C                                       Read and parse header
         DO 90 IREC = 1,100
            IF (IREC.EQ.1) GO TO 50
               CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
               IF (TERR.EQ.0) GO TO 30
                  WRITE (MSGTXT,1020) TERR
                  GO TO 890
 30            NC = 0
               CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
C                                       card loop
 50         NC = NC + 1
            IF (NC.GT.36) GO TO 90
C                                       card to history
            INC = (NC-1) * 80 + 1
            CARD = HDRBUF(INC:)
            IF (HISERR) GO TO 55
               ISHIS = CARD(1:8).EQ.'HISTORY'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.'COMMENT'
               IF (.NOT.ISHIS) ISHIS = CARD(1:8).EQ.' '
               IST = 1
               IF (ISHIS) IST = 9
               IF (CARD(1:4).NE.'END ') CALL HIAD80 (HLUN, IST, CARD,
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
               IF (X.EQ.DBLANK) GO TO 880
               IF (I.EQ.12) TABVER = X + 0.01
               IF (I.EQ.13) TABCNT = X + 0.01
               IF (I.EQ.14) TABWID = X + 0.01
               IF (I.EQ.15) TABCRD = X + 0.01
               GO TO 50
C                                       Got a string variable
 80            CALL GETSTR (CARD, 80, 68, NPNT, ISTR, NCHAR)
               NCHAR = MIN (NCHAR, 8)
               IF (I.EQ.11) TABNAM = ISTR(1:NCHAR)
               IF (I.LT.11) TTYPE(I) = ISTR(1:NCHAR)
               GO TO 50
 90         CONTINUE
         WRITE (MSGTXT,1090) ITAB
         GO TO 890
C                                       END card found
C                                       null table
 100     IF ((TABCNT.GT.0) .AND. (TABWID.GT.0)) GO TO 105
            WRITE (MSGTXT,1100) ITAB
            CALL MSGWRT (6)
            GO TO 170
C                                       illegal format
 105     IF ((TABCRD.GT.0) .AND. (TABCRD.LE.40)) GO TO 110
            WRITE (MSGTXT,1105) TABCRD, ITAB
            GO TO 890
C                                       A recognized type?
 110     NODATA = .TRUE.
         IF (TABNAM(1:4).NE.'AIPS') GO TO 125
            DO 115 IT = 1,NTYPES
               IF (CTYPES(IT).EQ.TABNAM(5:8)) GO TO 120
 115           CONTINUE
            GO TO 125
C                                       Yes: do it - CC files only
 120     IF ((IT.NE.1) .AND. (IT.NE.2)) GO TO 125
            NODATA = .FALSE.
C                                       Set correction for old CC
            XINC = 0.0
            IF (IT.NE.2) GO TO 124
               NDIM = CATBLK(KIDIM)
               DO 122 I = 1,NDIM
                  J = (I-1) * 2 + KHCTP
                  CALL H2CHR (4, 1, CATH(J), CHTM12)
                  EQUAL = LLCHAR(1:4).EQ.CHTM12(1:4)
                  IF (EQUAL) XINC = CATR(KRCIC+I-1)
 122              CONTINUE
               WRITE (MSGTXT,1122)
               CALL MSGWRT (2)
 124        CALL CCINI (LUN, TABWID, VOL, CNO, TABVER, CATBLK, BUFFER,
     *         IERR)
            IF (IERR.EQ.0) GO TO 125
               NODATA = .TRUE.
               WRITE (MSGTXT,1124) IERR
               CALL MSGWRT (7)
 125     IP = TABCRD
         DO 160 IRNO = 1,TABCNT
            DO 150 J = 1,TABWID
               IP = IP + 1
               IF (IP.LE.TABCRD) GO TO 140
                  NC = NC + 1
C                                       read a record
                  IF (NC.LE.36) GO TO 130
                     NC = 1
                     CALL TAPIO ('READ', FDVEC, TAPBUF, TBIND, TERR)
                     CALL ZC8CL (2880, 1, TAPBUF(TBIND), HDRBUF)
                     IF (TERR.EQ.0) GO TO 130
                        WRITE (MSGTXT,1020) TERR
                        GO TO 890
C                                       spread new card
 130              IP = 1
                  IF (NODATA) GO TO 140
                     INC = (NC-1) * 80 + 1
                     CARD = HDRBUF(INC:)
                     NPNT = 1
 140              IF (NODATA) GO TO 150
                     CALL GETNUM (CARD, 80, NPNT, X)
                     IF (X.EQ.DBLANK) GO TO 880
                     JJ = J
C                                       format correction
                     IF (IT.EQ.1) GO TO 145
                        IF (J.EQ.1) X = X + XINC
                        IF (J.LE.3) JJ = MOD (J, 3) + 1
 145                 RDATA(JJ) = X
 150              CONTINUE
               IF (NODATA) GO TO 160
                  CALL TABIO ('WRIT', 0, IRNO, RDATA, BUFFER, IERR)
                  IF (IERR.NE.0) GO TO 900
 160           CONTINUE
            IF (NODATA) GO TO 170
               CALL TABIO ('CLOS', 0, IRNO, RDATA, BUFFER, IERR)
               WRITE (MSGTXT,1160) 'CC', TABVER
               CALL MSGWRT (2)
               GO TO 200
C                                       Data ignored
 170        CONTINUE
               WRITE (MSGTXT,1170) ITAB
               CALL MSGWRT (2)
               IF (HISERR) GO TO 200
                  CALL HIAD80 (HLUN, 1, MSGTXT, HBUFF, IERR)
                  HISERR = IERR.NE.0
 200     CONTINUE
      IRET = 0
      GO TO 900
C
 880  MSGTXT = 'MLTABL: RANGE ERROR PARSING HEADER'
      IRET = 1
C
 890  CALL MSGWRT (8)
C                                       Read rest of tape
 900  IF (TERR.NE.4) CALL MLREOF (FDVEC, TBIND, UNKNWN, TAPBUF, IERR)
      IF (IERR.NE.0) IRET = 6
C                                       Close history
      CALL HICLOS (HLUN, T, HBUFF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CALRD  / HEADER FOR TABLE',I7)
 1020 FORMAT ('MLTABL: TAPE IO ERROR',I7)
 1090 FORMAT ('MLTABL: NO END TO TABLE HEADER #',I7)
 1100 FORMAT ('TABLE',I7,' HAS NO DATA')
 1105 FORMAT ('MLTABL: TABCARDS=',I7,' ILLEGAL')
 1122 FORMAT ('Correcting old CC format X positions')
 1124 FORMAT ('MLTABL: UNABLE TO CREATE EXTENSION FILE',I7)
 1160 FORMAT ('Extension file type ',A2,' version',I4,' written')
 1170 FORMAT ('CALRD / table',I7,' skipped')
      END
      SUBROUTINE FITRXM (IVOL, HLUN, HBUFF, NUMTAB, EOF, IERR)
C-----------------------------------------------------------------------
C  This routine will read all fits extension files associated with a map
C  and process the ones it recognizes (XTENSION = 'TABLES' as of now).
C  Inputs:
C     IVOL    I       Disk volume number of map and ext files.
C     HLUN    I       History file LUN. Already open.
C     HBUFF   I(256)  History file I/O buffer.
C  Outputs:
C     NUMTAB  I       Number of extension files found.
C     EOF     L       An end of file was read during processing.
C     IERR    I       Error code. 0=ok. >0 => Error
C                     -1 => too many tables for one output file.
C-----------------------------------------------------------------------
      INTEGER   MXTBKW, MAXTAB
C                                       MXTBKW=max. no. table keywords
      PARAMETER (MXTBKW = 1000)
C                                       MAXTAB = max number of tables.
      PARAMETER (MAXTAB=46000)
      CHARACTER KEYWRD(MXTBKW)*8, KEYCHR(MXTBKW)*8, TABLE*8, TAB3D(3)*8
      INTEGER   IVOL, HLUN, HBUFF(256), IERR, NUMTAB
      LOGICAL   EOF, T
      DOUBLE PRECISION    NBITS, AXCNT, KEYVAL(MXTBKW), KEYD
      REAL      KEYR(2)
      HOLLERITH KEYH(2)
      INTEGER   I, II, ICARD, INBLK, KEYTYP(MXTBKW), IVER, TABLUN,
     *   SRTORD, DATP(128,2), BUFFER(512), NUMKEY, KEYI, JERR, IKEY,
     *   KEYLOC(MXTBKW), KEYV(2*MXTBKW), LENKEY(5)
      LOGICAL   EXTEN, KEYL, DOHDR
      INCLUDE 'CALRD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DEHD.INC'
      INCLUDE 'INCS:DTHD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      EQUIVALENCE (KEYL, KEYI, KEYH, KEYR, KEYD)
      DATA TABLE, TAB3D /'TABLE', 'BINTABLE', 'A3DTABLE', '3D TABLE'/
      DATA TABLUN /29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Length of keyword values
      LENKEY(1) = NWDPDP
      LENKEY(2) = 1
      LENKEY(3) = 2
      LENKEY(4) = 1
      LENKEY(5) = 1
C                                       Open history
      CALL HIOPEN (HLUN, FVOL(1), FCNO(1), HBUFF, JERR)
      NUMTAB = 0
C                                       Loop for all FITS extensions.
      DO 200 I = 1,1000000
C                                       Process all required FITS table
C                                       cards.
         CALL EXTREQ (FDVEC, TBIND, TAPBUF, HDRBUF, ICARD, EXTEN, EOF,
     *      UNKNWN, IERR)
         IF (EOF) GO TO 900
         IF ((IERR.NE.0) .OR. (.NOT.EXTEN)) GO TO 900
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
         IF (EXTTYP.NE.TABLE) GO TO 40
            IF (NAXISI(1).LE.2880) GO TO 25
               WRITE (MSGTXT,1020) NAXISI(1)
               CALL MSGWRT (6)
               GO TO 100
C                                       initialize default values.
 25         CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 0, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, HDRBUF, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 30 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) CALL CHR2H (8, KEYCHR(II), 1, KEYH)
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 30            CONTINUE
C                                       Write keywords.
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL RWTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            NUMTAB = NUMTAB + 1
            GO TO 190
C                                       See if we have a 3-D table.
 40      IF ((EXTTYP.NE.TAB3D(1)) .AND. (EXTTYP.NE.TAB3D(2)) .AND.
     *      (EXTTYP.NE.TAB3D(3))) GO TO 100
C                                       initialize default values.
            CALL SETDEF
C                                       Process table file header.
            NUMKEY = MXTBKW
            CALL TABHDR (FDVEC, TBIND, ICARD, HLUN, HBUFF, 1, NUMKEY,
     *         KEYWRD, KEYVAL, KEYCHR, KEYTYP, TAPBUF, HDRBUF, IERR)
            IF (IERR.LT.0) GO TO 100
            IF (IERR.NE.0) GO TO 900
C                                       Normal table files. Special
C                                       processing if its an AIPS style
C                                       table.
            SRTORD = 0
            IF (ITYPE.NE.'UK') CALL ATCONV (NUMKEY, KEYWRD, KEYVAL,
     *         KEYTYP, KEYCHR, SRTORD)
C                                       Create and initialize the table
C                                       header with data in common.
            IVER = 0
            CALL MAKTAB (SRTORD, IVOL, CNO, IVER, CATBLK, TABLUN, DATP,
     *         BUFFER, JERR)
            IF (JERR.GT.0) GO TO 110
C                                       Trap filling up table type.
            EOF = IVER.GT.MAXTAB
C                                       Prepare keywords
            IKEY = 1
            DO 80 II = 1,NUMKEY
               KEYLOC(II) = IKEY
               IF (KEYTYP(II).EQ.2) KEYTYP(II) = 1
               IF (KEYTYP(II).EQ.1) KEYD = KEYVAL(II)
               IF (KEYTYP(II).EQ.3) CALL CHR2H (8, KEYCHR(II), 1, KEYH)
               IF (KEYTYP(II).EQ.4) KEYI = KEYVAL(II)
               IF (KEYTYP(II).EQ.5) KEYL = KEYVAL(II).GT.0.0D0
               CALL COPY (LENKEY(KEYTYP(II)), KEYD, KEYV(IKEY))
               IKEY = IKEY + LENKEY(KEYTYP(II))
 80            CONTINUE
C                                       Write keywords.
            CALL TABKEY ('WRIT', KEYWRD, NUMKEY, BUFFER, KEYLOC,
     *         KEYV, KEYTYP, JERR)
            IF (JERR.NE.0) THEN
               CALL TABIO ('CLOS', 0, II, BUFFER, BUFFER, JERR)
               GO TO 110
               END IF
C                                       Read the data from tape and
C                                       write to the table disk file.
            CALL R3DTAB (FDVEC, TBIND, DATP, NAXISI, BUFFER, TAPBUF,
     *         IERR)
            IF (IERR.NE.0) GO TO 900
            NUMTAB = NUMTAB + 1
            GO TO 190
C                                       Skip unknown extension file.
 100     CONTINUE
C                                       read rest header code
            DOHDR = .TRUE.
C                                       else header already read
 110        CALL SKPEXT (DOHDR, FDVEC, TBIND, HLUN, ICARD, INBLK, HBUFF,
     *         TAPBUF, HDRBUF, IERR)
            IF (IERR.NE.0) GO TO 900
C                                       Quit if filled up tables.
 190     IF (EOF) GO TO 900
C                                       Change /CFILES/ not to destroy
C                                       on ERROR
         FRW(NCFILE) = 1
 200     CONTINUE
C                                       Shouldn't get here.
      WRITE (MSGTXT,1200)
      CALL MSGWRT (6)
C                                       Close history
 900  CALL HICLOS (HLUN, T, HBUFF, JERR)
C                                       Trap too many tables
      IF ((IVER.GT.MAXTAB) .AND. (IERR.EQ.0)) IERR = -1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('FITRXM: ASCII TABLE ROW LENGTH',I7,' TOO LONG FOR ME')
 1200 FORMAT ('MORE THAN 32000 EXTENSION FILES. SOME NOT PROCESSED.')
      END


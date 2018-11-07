LOCAL INCLUDE 'OTFUV.INC'
C                                       Local include for OTFUV
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       inputs
      HOLLERITH XINFIL(12), XIN2FL(12), XOUNAM(3), XOUCLS(2)
      REAL      XBCNT, XECNT, XBIF, XEIF, XCOMP, XOUSEQ, XOUDSK, XXINC,
     *   XYINC, DOWT, XDOCON, XFQTOL, XADDAY, XCHSEL(3,10), XBCHAN,
     *   XECHAN
      COMMON /INPARM/ XINFIL, XIN2FL, XBCNT, XECNT, XBIF, XEIF, XCOMP,
     *   XOUNAM, XOUCLS, XOUSEQ, XOUDSK, XXINC, XYINC, DOWT, XDOCON,
     *   XFQTOL, XADDAY, XBCHAN, XECHAN, XCHSEL
C                                       internals
      INTEGER   SCRBUF(256), SEQOUT, DISKO, BCOUNT, ECOUNT, BIF, EIF,
     *   JBUFSZ, IBCNT, IECNT, NCHSEL, CHSEL(3,10), XINC, YINC, BCHAN,
     *   ECHAN, NIF, NWDP, NWSP, IBYTFL
      CHARACTER NAMOUT*12, CLAOUT*6, INFILE*48, IN2FIL*48, CFRAME*8
      REAL      BUFFER(UVBFSS), COSDEC, TIMAX, TIMERR, LTIME
      LOGICAL   COMPRS, SORTED, SWAPED, DOMSG
      COMMON /OTFUVP/ SCRBUF, BUFFER, SEQOUT, DISKO, BCOUNT, ECOUNT,
     *   NIF, BIF, EIF, JBUFSZ, COMPRS, IBCNT, IECNT, COSDEC, NCHSEL,
     *   CHSEL, SORTED, SWAPED, TIMAX, XINC, YINC, BCHAN, ECHAN, DOMSG,
     *   TIMERR, LTIME, NWDP, NWSP
      COMMON /OTFUVC/ INFILE, IN2FIL, NAMOUT, CLAOUT, CFRAME
C                                       SDD IO package
      INTEGER   DIRENT, NVDATA, NODATA, MAXIFS
      PARAMETER (MAXIFS = 8)
      PARAMETER (DIRENT = 5000)
      PARAMETER (NODATA = MAXCHA)
      DOUBLE PRECISION HEADS(192,5,MAXIFS), JD0, JD
      INTEGER   IHEADS(384,5,MAXIFS), IODATA(NODATA, MAXIFS),
     *   IGDATA(NODATA, MAXIFS), IODAT2(NODATA,MAXIFS),
     *   IGDAT2(NODATA,MAXIFS)
      INTEGER   DIREC(DIRENT, MAXIFS), IOBUF(256,2), ILUN(2), IIND(2),
     *   IIRET(2), IIREC(2), CUROFF(MAXIFS), CURGN(MAXIFS), CURSCN,
     *   CURREC, MAXREC, NCHAN, ALLREC, CUROF2(MAXIFS), CURGN2(MAXIFS),
     *   IFNUMB(MAXIFS), DIROFF(DIRENT,MAXIFS), DIRGN(DIRENT,MAXIFS),
     *   DSCAN(DIRENT), OSCAN(DIRENT), GSCAN(DIRENT), MAXSCN
      REAL      ODATA(NODATA, MAXIFS), GDATA(NODATA, MAXIFS),
     *   WEIGHT(MAXIFS), TIMES(DIRENT), TIMEO1, TIMEO2, TIMEG1, TIMEG2,
     *   ODATA2(NODATA,MAXIFS), GDATA2(NODATA,MAXIFS)
      EQUIVALENCE (IHEADS, HEADS), (IODATA, ODATA), (IGDATA, GDATA),
     *   (ODATA2, IODAT2), (GDATA2, IGDAT2)
      COMMON /SDDIO/ GDATA, ODATA, GDATA2, ODATA2, HEADS, DIREC, TIMES,
     *   IOBUF, JD0, JD, ILUN, IIND, IIRET, IIREC, CUROFF, CURGN,
     *   CURSCN, CURREC, MAXREC, NCHAN, WEIGHT, ALLREC, CUROF2, CURGN2,
     *   TIMEO1, TIMEO2, TIMEG1, TIMEG2, NVDATA, IFNUMB, DIROFF, DIRGN,
     *   DSCAN, OSCAN, GSCAN, MAXSCN, IBYTFL
      INCLUDE 'INCS:DSDD.INC'
C                                       standard includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM OTFUV
C-----------------------------------------------------------------------
C! Translates on-the-fly single-dish SDD format to AIPS UV file
C# Task General Singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2005, 2009, 2012, 2015
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
C   Task to read singledish data in UniPOPS internal format (SDD) and
C   write an AIPS UV data file in the single-dish format.
C   Limitations:
C      Now only does OTF line data from the 12m
C   Input adverbs:
C      INFILE......Input 12m sdd file (raw data file).
C      IN2FILE.....Input 12m gsdd file (gains file).
C      BCOUNT......First scan in OTF map which you want to process.
C      ECOUNT......Last scan in OTF map which you want to process.
C      BIF.........IF number to process.
C      EIF.........Last IF number to process
C      DOUVCOMP....If true (DOUVCOMP >= 0) the output data is written
C                  in compressed format which can result in a
C                  substantial reduction in disk space needed but
C                  a few tasks still cannot read this format.
C      OUTNAME.....Output UV file name (name).
C      OUTCLASS....Output UV file name (class).  Standard behavior
C                  with default = task name
C      OUTSEQ......Output UV file name (seq #).  0 => highest unique.
C      OUTDISK.....Output UV file disk drive #. 0 => highest w space
C                  for the initial file size.  Be careful to select a
C                  disk with plenty of space.  Unlike some AIPS
C                  tasks, OTFUV has to increase the size of the UV
C                  file after creation if there is more data than it
C                  guessed initially.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'OTFUV.INC'
      DATA PRGM /'OTFUV '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL OTFUVI (PRGM, IRET)
C                                       write the file
      CALL OTFUVF (IRET)
C                                       fill in history file
      CALL OTFUVH (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE OTFUVI (PRGM, IRET)
C-----------------------------------------------------------------------
C   gets input parameters for OTFUV, file creation has to wait
C   Inputs:
C      PRGM    C*6   Program name
C   Output:
C      IRET    I     Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      INTEGER   NPARM, IERR, IROUND, I
      INCLUDE 'OTFUV.INC'
      INCLUDE 'INCS:PUVD.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      SWAPED = .FALSE.
      DOMSG = .TRUE.
      NWSP = 4096 / NBITWD
      NWDP = NWSP / NWDPDP
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 73
      CALL GTPARM (PRGM, NPARM, RQUICK, XINFIL, SCRBUF, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, SCRBUF, IERR)
      IF (IRET.NE.0) GO TO 999
C                                       Crunch input parameters.
      SEQOUT = IROUND (XOUSEQ)
      DISKO = IROUND (XOUDSK)
      BCOUNT = IROUND (XBCNT)
      IF (BCOUNT.LE.0) BCOUNT = 1
      ECOUNT = IROUND (XECNT)
      IF (ECOUNT.LE.0) ECOUNT = BCOUNT + 100000
      IBCNT = ECOUNT
      IECNT = BCOUNT
      BIF = IROUND (XBIF)
      IF ((BIF.LT.1) .OR. (BIF.GT.99)) BIF = 1
      EIF = IROUND (XEIF)
      IF ((EIF.LT.BIF) .OR. (EIF.GT.99)) EIF = 99
      COMPRS = XCOMP.GT.0.0
      XINC = IROUND (XXINC)
      YINC = IROUND (XYINC)
      IF (YINC.LE.1) THEN
         YINC = 1
         XINC = 1
      ELSE
         IF (XINC.LT.1) XINC = YINC
         IF (XINC.GT.YINC) XINC = YINC
         END IF
      XXINC = XINC
      XYINC = YINC
      SORTED = .TRUE.
      TIMERR = 1000.
      LTIME = -1000.
      TIMAX = -1.0
      BCHAN = XBCHAN + 0.1
      ECHAN = XECHAN + 0.1
      IF (BCHAN.LE.0) BCHAN = 1
      IF (ECHAN.LE.BCHAN) ECHAN = MAXCHA
C                                       Characters
      CALL H2CHR (12, 1, XOUNAM, NAMOUT)
      CALL H2CHR (6, 1, XOUCLS, CLAOUT)
      CALL H2CHR (48, 1, XINFIL, INFILE)
      CALL H2CHR (48, 1, XIN2FL, IN2FIL)
      IF (INFILE.EQ.IN2FIL) THEN
         MSGTXT = 'INFILE = IN2FILE: NOT RIGHT!'
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      XADDAY = IROUND (XADDAY)
C                                       channel selection
      NCHSEL = 0
      CALL FILL (30, 0, CHSEL)
      DO 20 I = 1,10
          CHSEL(1,I) = IROUND (XCHSEL(1,I))
          CHSEL(2,I) = IROUND (XCHSEL(2,I))
          CHSEL(3,I) = IROUND (XCHSEL(3,I))
          IF ((CHSEL(2,I).GE.CHSEL(1,I)) .AND. (CHSEL(1,I).GT.0)) THEN
             NCHSEL = NCHSEL + 1
          ELSE
             GO TO 999
             END IF
 20       CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
      END
      SUBROUTINE OTFUVF (IRET)
C-----------------------------------------------------------------------
C   Reads the input SDD data, creates and writes the output UV file
C   In/Out:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NIOUT, NUMVIS, XCOUNT, IPTRO, NIOLIM, LUNO, INDO, NCOPY,
     *   NCORO, BO, KBIND, ILENBU, NREC, CRAZY, FSTVIS, I, IIF, NWORDS,
     *   ITEMP
      LONGINT   TBOFF, VDOFF, IP
      CHARACTER PHFILE*48
      LOGICAL   T, F
      DOUBLE PRECISION JDOLD
      INCLUDE 'OTFUV.INC'
      INCLUDE 'INCS:DDCH.INC'
      REAL      TBUFF(2), VDATA(2), CTIME, RPARMS(8,MAXIFS)
      DATA LUNO, BO /16, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Init the SDD IO
      NUMVIS = 0
      NVDATA = 0
      ILUN(1) = 17
      ILUN(2) = 18
      IIREC(1) = -1
      IIREC(2) = -1
      CALL FILL (MAXIFS, -1, CUROFF)
      CALL FILL (MAXIFS, -1, CURGN)
      CALL FILL (MAXIFS, -1, CUROF2)
      CALL FILL (MAXIFS, -1, CURGN2)
      CURSCN = -1
      CURREC = -XINC
      MAXREC = -1
      ALLREC = 0
      NCHAN = 0
      I = DIRENT * MAXIFS
      CALL FILL (I, 0, DIREC)
      CALL FILL (I, 0, DIROFF)
      CALL FILL (I, 0, DIRGN)
      CALL FILL (DIRENT, 0, DSCAN)
      CALL FILL (DIRENT, 0, OSCAN)
      CALL FILL (DIRENT, 0, GSCAN)
      CALL RFILL (DIRENT, -999.0, TIMES)
      CALL OTFUVD (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Allocate memory
      NWORDS = 3 * NCHAN * NIF
      NWORDS = (NWORDS - 1)/1024 + 1
      CALL ZMEMRY ('GET ', 'OTFUVF', NWORDS, TBUFF, TBOFF, IRET)
      IF (IRET.EQ.0) THEN
         NWORDS = NVDATA * NIF
         NWORDS = (NWORDS - 1)/1024 + 1
         CALL ZMEMRY ('GET ', 'OTFUVF', NWORDS, VDATA, VDOFF, IRET)
         NVDATA = (1024 * NWORDS) / NIF
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Initial call - make UV header
      JDOLD = JD0
      CALL OTFUVG (NUMVIS, RPARMS, NCHAN, TBUFF(1+TBOFF), NVDATA,
     *   VDATA(1+VDOFF), IRET)
      IF (IRET.GT.0) GO TO 999
C                                       create the file or decide on
C                                       catcatenation
      CALL OTFUVM (FSTVIS, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       fix directory times
      IF (JDOLD.NE.JD0) THEN
         TIMEO1 = TIMEO1 + JDOLD - JD0
         TIMEO2 = TIMEO2 + JDOLD - JD0
         TIMEG1 = TIMEG1 + JDOLD - JD0
         TIMEG2 = TIMEG2 + JDOLD - JD0
         DO 20 I = 1,MAXSCN
            TIMES(I) = TIMES(I) + JDOLD - JD0
 20         CONTINUE
         END IF
      XCOUNT = FSTVIS
C                                       make an antenna file
C                                       do not die on error
      CALL OTFANT
C                                       save output names
      SEQOUT = CATBLK(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH(KHIMN), NAMOUT)
      CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLAOUT)
C                                       interpret the header
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHFILE, T, F, F, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'OPEN'
         GO TO 990
         END IF
C                                       Get last time
      IF ((SORTED) .AND. (FSTVIS.GT.0)) THEN
         ILENBU = 0
         I = FSTVIS - 1
         CALL UVINIT ('READ', LUNO, INDO, 1, I, LREC, ILENBU, JBUFSZ,
     *      BUFFER, BO, KBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'INIT'
            GO TO 990
            END IF
         CALL UVDISK ('READ', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, 'WRIT'
            GO TO 990
            END IF
         TIMAX = BUFFER(KBIND+2)
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CRAZY = 4000000 + FSTVIS
      CALL UVINIT ('WRIT', LUNO, INDO, CRAZY, FSTVIS, LREC, ILENBU,
     *   JBUFSZ, BUFFER, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'INIT'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NCORO = (LREC - NRPARM) / CATBLK(KINAX)
      NCOPY = LREC - NRPARM
C                                       Loop
 100  CONTINUE
C                                       Get next sample
         NUMVIS = NUMVIS + 1
         CALL OTFUVG (NUMVIS, RPARMS, NCHAN, TBUFF(1+TBOFF), NVDATA,
     *      VDATA(1+VDOFF), IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Got datum
         IF (IRET.EQ.0) THEN
            DO 150 IIF = 1,NIF
               CALL RCOPY (6, RPARMS(1,IIF), BUFFER(IPTRO))
               IP = 3 * NCHAN * (IIF - 1) + 1 + TBOFF
               IF (COMPRS) THEN
                  ITEMP = BYTFLP
                  BYTFLP = IBYTFL
                  CALL ZUVPAK (NCORO, TBUFF(IP), BUFFER(IPTRO+6),
     *               BUFFER(IPTRO+NRPARM))
                  BYTFLP = ITEMP
               ELSE
                  CALL RCOPY (NCOPY, TBUFF(IP), BUFFER(IPTRO+NRPARM))
                  END IF
               XCOUNT = XCOUNT + 1
               CTIME = BUFFER(IPTRO+2)
               IF (CTIME-TIMAX.LE.-2.E-7) THEN
                  SORTED = .FALSE.
                  TIMERR = MIN (TIMERR, CTIME-TIMAX)
                  END IF
               TIMAX = MAX (CTIME, TIMAX)
               IPTRO = IPTRO + LREC
               NIOUT = NIOUT + 1
C                                       Expand file
               IF (XCOUNT.GT.CATBLK(KIGCN)) THEN
                  NREC = (10100 * LREC * 2) / 512 + 1
                  CALL ZEXPND (LUNO, DISKO, PHFILE, NREC, IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1010) IRET, 'EXPAND'
                     GO TO 990
                     END IF
                  CALL ZEXIST (DISKO, PHFILE, NREC, IRET)
                  CATBLK(KIGCN) = (NREC * 256.0D0) / LREC
                  END IF
C                                       Write vis record.
               IF (NIOUT.GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO, INDO, BUFFER, NIOUT, KBIND,
     *               IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1010) IRET, 'WRIT'
                     GO TO 990
                     END IF
                  IPTRO = KBIND
                  NIOUT = 0
                  END IF
 150           CONTINUE
C                                       Next vis.
            GO TO 100
            END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFFER, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET, 'FLSH'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close file
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 995
C
 990  CALL MSGWRT (8)
C
 995  CALL ZMEMRY ('QUIT', 'OTFUVF', NWORDS, TBUFF, TBOFF, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVF: ERROR',I5,' ALLOCATING DYNAMIC MEMORY')
 1010 FORMAT ('OTFUVF: ERROR',I4,1X,A,'ING OUTPUT UV FILE')
      END
      SUBROUTINE OTFUVH (IRET)
C-----------------------------------------------------------------------
C   Adds history file and info to the output UV file
C   In/Out:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNH, I, BUFFH(256), IERR, ITRIM, DATE(3), TIME(3)
      CHARACTER LINE*72, ATIME*8, ADATE*12
      INCLUDE 'OTFUV.INC'
      DATA LUNH /28/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUNH, DISKO, FCNO(1), CATBLK, BUFFH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (LINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       AIPS release
      WRITE (LINE,1011) TSKNAM, RLSNAM
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       output names
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUNH,
     *   BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       inputs
      I = ITRIM (INFILE)
      WRITE (LINE,1020) TSKNAM, INFILE(:I)
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
      I = ITRIM (IN2FIL)
      WRITE (LINE,1021) TSKNAM, IN2FIL(:I)
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (LINE,1025) TSKNAM, BCOUNT
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (LINE,1026) TSKNAM, ECOUNT
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
      DO 20 I = 1,NIF
         WRITE (LINE,1027) TSKNAM, IFNUMB(I)
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 100
 20      CONTINUE
      IF (NCHSEL.GT.0) THEN
         DO 30 I = 1,NCHSEL
            WRITE (LINE,1028) TSKNAM, CHSEL(1,I), CHSEL(2,I), CHSEL(3,I)
            CALL HIADD (LUNH, LINE, BUFFH, IERR)
            IF (IERR.NE.0) GO TO 100
 30         CONTINUE
         END IF
C                                       averaging
      WRITE (LINE,1030) TSKNAM, XINC, YINC
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       time smooth
      IF (DOWT.GT.0.0) THEN
         WRITE (LINE,1035) TSKNAM
      ELSE
         WRITE (LINE,1036) TSKNAM
         END IF
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       channels
      WRITE (LINE,1037) TSKNAM, BCHAN, ECHAN
      CALL HIADD (LUNH, LINE, BUFFH, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUNH, .TRUE., BUFFH, IERR)
C                                       Update CATBLK.
      IF (SORTED) THEN
         MSGTXT = 'Data are in time order'
         CALL MSGWRT (2)
         CALL CHR2H (2, 'T ', 1, CATH(KITYP))
      ELSE
         MSGTXT = 'WARNING: DATA ARE NOT IN TIME ORDER'
         CALL MSGWRT (6)
         TIMERR = -86400 * TIMERR
         WRITE (MSGTXT,1100) TIMERR
         CALL MSGWRT (6)
         CALL CHR2H (2, '  ', 1, CATH(KITYP))
         END IF
      CALL CATIO ('UPDT', DISKO, FCNO(1), CATBLK, 'REST', SCRBUF, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVH: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'/ AIPS Release = ''',A7,' ''')
 1020 FORMAT (A6,'INFILE =''',A,''' / data')
 1021 FORMAT (A6,'IN2FILE =''',A,''' / gain')
 1025 FORMAT (A6,'BCOUNT =',I6,5X,'/ first scan to include')
 1026 FORMAT (A6,'ECOUNT =',I6,5X,'/ last scan to include')
 1027 FORMAT (A6,'IF =',I3,12X,'/  IF number included')
 1028 FORMAT (A6,'CHANSEL =',3I6,5X,'/ flag channel n1 to n2 by n3')
 1030 FORMAT (A6,'XINC =',I4,' YINC =',I4,3X,
     *   '/ output every X times averaging Y samples')
 1035 FORMAT (A6,'DOWEIGHT = T',5X,'/ Offs and gains time smoothed')
 1036 FORMAT (A6,'DOWEIGHT = F',5X,'/ Formal offs and gains used')
 1037 FORMAT (A6,'BCHAN =',I5,' ECHAN =',I5,5X,'/ channels included')
 1100 FORMAT ('MAXIMUM TIME MISORDER =',F11.4,' SECONDS')
      END
      SUBROUTINE OTFUVO (NF, NR, REC, IRET)
C-----------------------------------------------------------------------
C   does actual read IO from disk file
C   Inputs:
C      NF     I        File number: 1 or 2
C      NR     I        Desired record number (512-byte records)
C   Outputs:
C      REC    I(128)   Data record
C      IRET   I        I/O error: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   NF, NR, REC(128), IRET
C
      INTEGER   IP, IR
      CHARACTER FILET(2)*4
      INCLUDE 'OTFUV.INC'
      DATA FILET /'DATA','GAIN'/
C-----------------------------------------------------------------------
      IRET = -1
C                                       ZFIO record number
      IF ((NF.GE.1) .AND. (NF.LE.2) .AND. (NR.GT.0)) THEN
         IR = (NR-1)/2 + 1
C                                       don't have it yet
         IF (IR.NE.IIREC(NF)) THEN
            IIREC(NF) = IR
            CALL ZFIO ('READ', ILUN(NF), IIND(NF), IIREC(NF),
     *         IOBUF(1,NF), IIRET(NF))
            IRET = IIRET(NF)
            IF ((IRET.NE.0) .AND. (IRET.LT.1128)) THEN
               WRITE (MSGTXT,1000) IRET, FILET(NF), NR
               GO TO 990
               END IF
            END IF
C                                       copy the data
         IP = 1 + 128 * MOD (NR-1, 2)
         CALL COPY (128, IOBUF(IP,NF), REC)
         IRET = 0
         IF ((IP.GT.1) .AND. (IIRET(NF).GT.1000) .AND.
     *      (IIRET(NF).LT.1256)) THEN
            IRET = 4
            WRITE (MSGTXT,1010) NR
            GO TO 990
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVO: ERROR',I5,' READING SDD ',A,' FILE RECORD',I8)
 1010 FORMAT ('OTFUVO: ATTEMPT TO READ PAST END-OF-FILE RECORD',I8)
      END
      SUBROUTINE OTFUVG (NUMVIS, RPARMS, NC, VIS, NV, VDATA, IRET)
C-----------------------------------------------------------------------
C   Data reader and interpreter for OTFUV
C   Inputs:
C      NUMVIS   I       0 => fill in CATBLK, using 1st scan, open inputs
C                      >0 => return 1 Vis record
C                      <0 => close down
C   Outputs:
C      RPARMS   R(*)   Random parameters (except weight and scale)
C      VIS      R(*)   Data (Flux, 0, weight) by nchan
C   In/Out:
C      IRET     I      0 => ok, > 0 => die, < 0 => out of data
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NC, NV, IRET
      REAL      VDATA(NV,*), RPARMS(8,*), VIS(3,NC,*)
C
      INTEGER   I, J, NRP, IT(6), IP, LINC, K, II, IIF
      REAL      S1, S2, S3, WO1, WO2, WG1, WG2, DOFF, DGAIN
      CHARACTER CTEMP*8, RTYPE(8)*8, CTYPE(5)*8
      DOUBLE PRECISION DTEMP
      INCLUDE 'OTFUV.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA RTYPE /'RA', 'DEC', 'TIME1', 'BEAM', 'SCAN', 'SAMPLE',
     *   'WEIGHT', 'SCALE'/
      DATA CTYPE /'COMPLEX', 'STOKES', 'FREQ', 'RA', 'DEC'/
      DATA IT /6*0/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Initial call
      IF (NUMVIS.EQ.0) THEN
C                                       Get first data
         CALL OTFUVR (NV, VDATA, IRET)
         IF (IRET.LT.0) THEN
            MSGTXT = 'NO DATA FOUND ON FIRST READ - DIE'
            CALL MSGWRT (8)
            IRET = 8
            END IF
         IF (IRET.NE.0) GO TO 999
C                                       Init the header
         CALL CATINI (CATBLK)
         NRP = 6
         IF (COMPRS) NRP = NRP + 2
         CATBLK(KIPCN) = NRP
         DO 10 I = 1,NRP
            CTEMP = RTYPE(I)
            J = KHPTP + (I-1) * 2
            CALL CHR2H (8, CTEMP, 1, CATH(J))
 10         CONTINUE
         CATBLK(KIDIM) = 5
         DO 20 I = 1,5
            CTEMP = CTYPE(I)
            J = KHCTP + (I-1) * 2
            CALL CHR2H (8, CTEMP, 1, CATH(J))
 20         CONTINUE
         IF (.NOT.COMPRS) CATBLK(KINAX) = 3
         CATD(KDCRV) = 1.0D0
         CATD(KDCRV+1) = 1.0D0
C                                       Miscellaneous items.
C                                       Sort order ('**'=>unsorted)
         CALL CHR2H (2, '  ', 1, CATH(KITYP))
C                                       Units
         CTEMP = 'K'
         CALL CHR2H (8, CTEMP, 1, CATH(KHBUN))
C                                       Number of vis.
         CATBLK(KIGCN) = 10000
C                                       NOW - From the data:
C                                       Observing date.
         DTEMP = HEADS(SUTDA,1,1)
         IT(1) = DTEMP
         DTEMP = 100.0D0 * (DTEMP - IT(1))
         IT(2) = DTEMP
         DTEMP = 100.0D0 * (DTEMP - IT(2))
         IT(3) = DTEMP + 0.1
         WRITE (CTEMP,1030) IT(1), IT(2), IT(3)
         CALL CHR2H (8, CTEMP, 1, CATH(KHDOB))
         CALL CHR2H (8, CTEMP, 1, CATH(KHDMP))
         CALL JULDAY (CTEMP, JD0)
C                                       Coordinate frame
         CALL DL2CHR (HEADS(SFRAM,1,1), SWAPED, CFRAME)
         IF (CFRAME(:5).NE.'EPHEM') DOMSG = .FALSE.
C                                       Telescope.
         CALL DL2CHR (HEADS(STELE,1,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHTEL))
C                                       Receiver
         CALL DL2CHR (HEADS(SFRNT,1,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHINS))
C                                       Object.
         CALL DL2CHR (HEADS(SOBJE,1,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHOBJ))
C                                       Observer's name.
         CALL DL2CHR (HEADS(SOBSI,1,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHOBS))
C                                       Epoch.
         CATR(KREPO) = HEADS(SEPOC,1,1)
C                                       "Old" (observed) position.
         CATD(KDORA) = HEADS(SXSOU,1,1)
         CATD(KDODE) = HEADS(SYSOU,1,1)
         CATD(KDCRV+3) = HEADS(SXSOU,1,1)
         CATD(KDCRV+4) = HEADS(SYSOU,1,1)
         COSDEC = COS (CATD(KDODE) * DG2RAD)
         IF (COSDEC.EQ.0.0) COSDEC = 1.0
C                                       Rest Frequency
         CATD(KDRST) = HEADS(SRSTF,1,1) * 1.0D6
C                                       Alternate ref. value & pixel
         CATD(KDARV) = HEADS(SX0,1,1) * 1.0D3
         CATR(KRARP) = HEADS(SREFP,1,1) - BCHAN + 1
         CALL DL2CHR (HEADS(SVELD,1,1), SWAPED, CTEMP)
         CATBLK(KIALT) = 0
         IF (CTEMP(:4).EQ.'RADI') CATBLK(KIALT) = 256
         IF (CTEMP(5:7).EQ.'LSR') THEN
            CATBLK(KIALT) = CATBLK(KIALT)+ 1
         ELSE IF (CTEMP(5:7).EQ.'HEL') THEN
            CATBLK(KIALT) = CATBLK(KIALT)+ 2
         ELSE
            CATBLK(KIALT) = CATBLK(KIALT)+ 3
            END IF
C                                       Freq axis
         CATD(KDCRV+2) = HEADS(SOBSF,1,1) * 1.0D6
         IF (BCHAN.GE.NCHAN) THEN
            BCHAN = 1
            ECHAN = NCHAN
            END IF
         ECHAN = MIN (NCHAN, ECHAN)
         CATBLK(KINAX+2) = ECHAN - BCHAN + 1
         CATR(KRCIC+2) = HEADS(SFRQR,1,1) * 1.0D6
         CATR(KRCRP+2) = HEADS(SREFP,1,1) - BCHAN + 1
C                                       Convolution size: 1.2 lambda/D
         CATR(KRBMJ) = RAD2DG * VELITE / 1.0D1 / CATD(KDCRV+2)
         CATR(KRBMN) = CATR(KRBMJ)
C                                       Image name parameters
         IF (NAMOUT.EQ.' ') CALL H2CHR (8, 1, CATH(KHOBJ), NAMOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         IF (CLAOUT.EQ.' ') CLAOUT = TSKNAM
         CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
         CALL CHR2H (2, 'UV', KHPTYO, CATR(KHPTY))
         CATBLK(KIIMS) = SEQOUT
C                                       Later calls for data
      ELSE IF (NUMVIS.GT.0) THEN
C                                       Need more data
         CURREC = MAX (0, CURREC+XINC)
         IF (CURREC.GE.MAXREC) THEN
            CALL OTFUVR (NV, VDATA, IRET)
            IF (IRET.NE.0) GO TO 999
            CURREC = 0
            END IF
         IF (CURREC.EQ.0) THEN
            DTEMP = HEADS(SUTDA,1,1)
            IT(1) = DTEMP
            DTEMP = 100.0D0 * (DTEMP - IT(1))
            IT(2) = DTEMP
            DTEMP = 100.0D0 * (DTEMP - IT(2))
            IT(3) = DTEMP + 0.1
            CALL FILL (3, 0, IT(4))
            CALL DAT2JD (IT, JD)
            DO 115 IIF = 1,NIF
C                                       Test coordinate
               IF (DOMSG) THEN
                  IF ((ABS(CATD(KDORA)-HEADS(SXSOU,1,IIF)).GE.1.0) .OR.
     *               (ABS(CATD(KDODE)-HEADS(SYSOU,1,IIF)).GE.1.0)) THEN
                     DOMSG = .FALSE.
                     WRITE (MSGTXT,1100) 'RA ', CATD(KDORA),
     *                  HEADS(SXSOU,1,IIF)
                     CALL MSGWRT (7)
                     WRITE (MSGTXT,1100) 'DEC', CATD(KDODE),
     *                  HEADS(SYSOU,1,IIF)
                     CALL MSGWRT (7)
                     END IF
                  END IF
C                                       Planets -> use first coord
               IF (CFRAME(:5).EQ.'EPHEM') THEN
                  HEADS(SXSOU,1,IIF) = CATD(KDORA)
                  HEADS(SYSOU,1,IIF) = CATD(KDODE)
                  END IF
C                                       Noise based weight
               WEIGHT(IIF) = HEADS(STSYS,1,IIF) / 1000.0D0
               IF (WEIGHT(IIF).LE.0.0) WEIGHT(IIF) = 1.0
               IF (HEADS(SSAMP,1,IIF).GT.0.0D0) THEN
                  WEIGHT(IIF) = (10.0 * HEADS(SSAMP,1,IIF)) /
     *               (WEIGHT(IIF) * WEIGHT(IIF))
               ELSE
                  WEIGHT(IIF) = 1.0 / (WEIGHT(IIF) * WEIGHT(IIF))
                  END IF
C                                       Defend ourselves
               DO 110 I = BCHAN,ECHAN
                  IF (ODATA(I,IIF).EQ.0.0) ODATA(I,IIF) = 1.0
                  IF (GDATA(I,IIF).EQ.0.0) GDATA(I,IIF) = 1.0
                  IF (ODATA2(I,IIF).EQ.0.0) ODATA2(I,IIF) = 1.0
                  IF (GDATA2(I,IIF).EQ.0.0) GDATA2(I,IIF) = 1.0
 110              CONTINUE
 115           CONTINUE
            END IF
         LINC = MIN (MAXREC-CURREC, YINC)
         DO 150 IIF = 1,NIF
            S1 = 0.
            S2 = 0.
            S3 = 0.
            IP = MAXREC * NCHAN + CURREC
            DO 120 K = 1,LINC
               S1 = S1 + VDATA(IP+K,IIF)
               S2 = S2 + VDATA(IP+K+MAXREC,IIF)
               S3 = S3 + VDATA(IP+K+MAXREC+MAXREC,IIF)
 120           CONTINUE
            RPARMS(3,IIF) = S1 / 3.6E6 / LINC
            RPARMS(3,IIF) = JD - JD0 + RPARMS(3,IIF)/24.0D0
            IF (LTIME.EQ.-1000.) LTIME = RPARMS(3,IIF)
C                                       day changed
            IF (LTIME-RPARMS(3,IIF).GT.0.8) THEN
               JD = JD + 1.0D0
               RPARMS(3,IIF) = RPARMS(3,IIF) + 1.0
               END IF
            LTIME = RPARMS(3,IIF)
            RPARMS(1,IIF) = HEADS(SXSOU,1,IIF) + S2 / COSDEC / LINC
            RPARMS(2,IIF) = HEADS(SYSOU,1,IIF) + S3 / LINC
            ALLREC = ALLREC + 1
            IF ((DOWT.GT.0.0) .AND. (TIMEO2.GT.TIMEO1)) THEN
               WO2 = (RPARMS(3,IIF) - TIMEO1) / (TIMEO2 - TIMEO1)
            ELSE
               WO2 = 0.0
               END IF
            WO1 = 1.0 - WO2
            IF ((DOWT.GT.0.0) .AND. (TIMEG2.GT.TIMEG1)) THEN
               WG2 = (RPARMS(3,IIF) - TIMEG1) / (TIMEG2 - TIMEG1)
            ELSE
               WG2 = 0.0
               END IF
            WG1 = 1.0 - WG2
C                                       one-beam system
            RPARMS(4,IIF) = 257.0 * IFNUMB(IIF)
            IP = HEADS(SSCAN,1,IIF) + 0.01
            RPARMS(5,IIF) = IP
            RPARMS(6,IIF) = CURREC + (LINC + 1) / 2
            IP = CURREC * NCHAN
            DO 130 I = BCHAN,ECHAN
               S1 = 0.0
               S2 = 0.0
               J = IP
               DO 125 K = 1,LINC
                  IF (VDATA(J+I,IIF).NE.1.0E-20) THEN
                     S1 = S1 + VDATA(J+I,IIF)
                     S2 = S2 + 1.0
                     END IF
                  J = J + NCHAN
 125              CONTINUE
               II = I - BCHAN + 1
               IF (S2.GT.0.5) THEN
                  IF (DOWT.GT.0.0) THEN
                     DOFF = WO1 * ODATA(I,IIF) + WO2 * ODATA2(I,IIF)
                     DGAIN = WG1 * GDATA(I,IIF) + WG2 * GDATA2(I,IIF)
                  ELSE
                     DOFF = ODATA(I,IIF)
                     DGAIN = GDATA(I,IIF)
                     END IF
                  VIS(1,II,IIF) = ((S1/S2 - DOFF) / DOFF) * DGAIN
                  VIS(3,II,IIF) = WEIGHT(IIF) * S2
               ELSE
                  VIS(1,II,IIF) = 0.0
                  VIS(3,II,IIF) = -WEIGHT(IIF)
                  END IF
               VIS(2,II,IIF) = 0.0
 130           CONTINUE
C                                       flagging
            IF (NCHSEL.GT.0) THEN
               DO 140 J = 1,NCHSEL
                  DO 135 I = CHSEL(1,J),CHSEL(2,J),CHSEL(3,J)
                     IF ((I.GE.BCHAN) .AND. (I.LE.ECHAN)) THEN
                        II = I - BCHAN + 1
                        VIS(3,II,IIF) = -ABS (VIS(3,II,IIF))
                        END IF
 135                 CONTINUE
 140              CONTINUE
               END IF
 150        CONTINUE
C                                       Final call
      ELSE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I4.4,2I2.2)
 1100 FORMAT ('PROBABLE ERROR: MERGING ',A,' COORDINATES',2F12.7)
      END
      SUBROUTINE OTFUVD (IRET)
C-----------------------------------------------------------------------
C   opens the data and gains file and reads in their directories for the
C   specified range in BCOUNT, ECOUNT, and IF = BIF-EIF.
C   Output:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, J, IBUF(128), MAXDIR, ID, IR, LS, II, JJ, ISC, NREC,
     *   IT(6), IDR, LIF, IIF, FOUND(100), LOFF, LGAIN, MNOFF, MXOFF,
     *   MNGAIN, MXGAIN
      DOUBLE PRECISION DTEMP, FREQS(100), FREQB
      LOGICAL   T, F, FIRST, LINUXS
      REAL      RBUF(128), RTEMP, INCRS(100), INCRB
      CHARACTER INF*48, FTYPE(2)*4, SCNTYP*8
      INCLUDE 'OTFUV.INC'
      DATA FTYPE /'DATA','GAIN'/
      DATA T,F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FILL (100, 0, FOUND)
      FIRST = .TRUE.
C                                       open data and gains files
      INF = INFILE
      DO 10 I = 1,2
         CALL ZOPEN (ILUN(I), IIND(I), 1, INF, F, F, T, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'OPEN', FTYPE(I)
            GO TO 990
            END IF
         INF = IN2FIL
 10      CONTINUE
      SWAPED = .FALSE.
      CALL FILL (3, 0, IT(4))
C                                       build up the directory
C                                       read and check the bootstrap
      IDR = 1
      CALL OTFUVO (1, IDR, IBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Swapped bytes ??
      IBYTFL = BYTFLP
      IF ((IBUF(3).EQ.512) .OR. (IBUF(4).EQ.64)) THEN
         LINUXS = (IBYTFL.EQ.3)
         IF (LINUXS) BYTFLP = 3 - BYTFLP
      ELSE
         LINUXS = (IBYTFL.EQ.0)
         IF (LINUXS) BYTFLP = 3 - BYTFLP
         CALL ZI32IL (128, 1, IBUF, IBUF)
         SWAPED = (IBUF(3).EQ.512) .AND. (IBUF(4).EQ.64)
         IF (.NOT.SWAPED) CALL ZILI32 (128, IBUF, 1, IBUF)
         END IF
      IF ((IBUF(3).NE.512) .OR. (IBUF(4).NE.64)) THEN
         WRITE (MSGTXT,1010) I, IBUF(3), IBUF(4)
         IRET = 1
         GO TO 990
         END IF
      MAXDIR = IBUF(5)
      IF ((IBUF(7).NE.0) .OR. (IBUF(8).NE.1)) THEN
         WRITE (MSGTXT,1011) I, IBUF(7), IBUF(8)
         CALL MSGWRT (6)
         END IF
C                                       read directory, set IFs
      IDR = 1
      DO 30 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFUVO (1, IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZBFLIP (4, 128, RBUF, RBUF)
            END IF
         DO 25 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
               IF ((LIF.GE.BIF) .AND. (LIF.LE.EIF) .AND.
     *            (LS.GE.BCOUNT) .AND. (LS.LE.ECOUNT) .AND.
     *            (FOUND(LIF).EQ.0)) THEN
C                                       read the header
                  IR = IBUF(16*J-15)
                  DO 20 II = 1,3
                     JJ = NWSP * (II - 1) + 1
                     CALL OTFUVO (1, IR, IHEADS(JJ,1,1), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 20                  CONTINUE
                  IF (FIRST) CALL SDDPTS (IBYTFL, HEADS)
                  FIRST = .FALSE.
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS, HEADS)
                  CALL DL2CHR (HEADS(SOBSM,1,1), SWAPED, SCNTYP)
C                                       OTF on data
                  IF (SCNTYP.EQ.'LINEOTF ') THEN
                     FOUND(LIF) = 1
                     FREQS(LIF) = HEADS(SOBSF,1,1)
                     INCRS(LIF) = HEADS(SFRQR,1,1)
                     END IF
                  END IF
               END IF
 25         CONTINUE
 30      CONTINUE
C                                       Find included IFs
      NIF = 0
      II = BIF
      JJ = EIF
      DO 40 LIF = II,JJ
         IF (FOUND(LIF).GT.0) THEN
            IF (NIF.EQ.0) THEN
               BIF = LIF
               FREQB = FREQS(LIF)
               INCRB = INCRS(LIF)
               IFNUMB(1) = BIF
               NIF = 1
               EIF = BIF
               WRITE (MSGTXT,1020) BIF
               CALL MSGWRT (2)
            ELSE IF (NIF.LT.MAXIFS) THEN
               IF ((ABS(FREQS(LIF)-FREQB).LT.10.) .AND.
     *            (ABS(INCRS(LIF)-INCRB).LT.1.E-3)) THEN
                  NIF = NIF + 1
                  IFNUMB(NIF) = LIF
                  EIF = LIF
                  WRITE (MSGTXT,1021) LIF
                  CALL MSGWRT (2)
               ELSE
                  FOUND(LIF) = -1
                  WRITE (MSGTXT,1022) LIF
                  CALL MSGWRT (6)
                  WRITE (MSGTXT,1023) FREQS(LIF), INCRS(LIF), FREQB,
     *               INCRB
                  CALL MSGWRT (6)
                  END IF
            ELSE
               FOUND(LIF) = -1
               WRITE (MSGTXT,1024) LIF
               CALL MSGWRT (6)
               END IF
            END IF
 40      CONTINUE
C                                       read directory for pointers
      IDR = 1
      MAXSCN = 0
      MNGAIN = 3276800
      MNOFF  = 3276800
      MXGAIN = 0
      MXOFF  = 0
      DO 80 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFUVO (1, IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZBFLIP (4, 128, RBUF, RBUF)
            END IF
         DO 70 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
               IF ((FOUND(LIF).GT.0) .AND. (LS.GE.BCOUNT) .AND.
     *            (LS.LE.ECOUNT)) THEN
                  IIF = 0
                  DO 50 II = 1,NIF
                     IF (LIF.EQ.IFNUMB(II)) IIF = II
 50                  CONTINUE
                  IF (IIF.LE.0) THEN
                     WRITE (MSGTXT,1050) LIF
                     CALL MSGWRT (6)
                     GO TO 70
                     END IF
C                                       read the header
                  IR = IBUF(16*J-15)
                  DO 60 II = 1,3
                     JJ = NWSP * (II - 1) + 1
                     CALL OTFUVO (1, IR, IHEADS(JJ,1,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 60                  CONTINUE
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,1,IIF),
     *               HEADS(1,1,IIF))
                  CALL DL2CHR (HEADS(SOBSM,1,IIF), SWAPED, SCNTYP)
                  LOFF = HEADS(SOFFS,1,IIF) + 0.001
                  LGAIN = HEADS(SGAIN,1,IIF) + 0.001
                  IR = IBUF(16*J-15)
C                                       OTF on data
                  IF (SCNTYP.EQ.'LINEOTF ') THEN
                     IF ((MAXSCN.EQ.0) .OR. (DSCAN(MAXSCN).NE.LS)) THEN
                        MAXSCN = MAXSCN + 1
                        DSCAN(MAXSCN) = LS
                        OSCAN(MAXSCN) = LOFF
                        GSCAN(MAXSCN) = LGAIN
                        MNOFF = MIN (MNOFF, LOFF)
                        MXOFF = MAX (MXOFF, LOFF)
                        MNGAIN = MIN (MNGAIN, LGAIN)
                        MXGAIN = MAX (MXGAIN, LGAIN)
C                                       get time
                        DTEMP = HEADS(SUTDA,1,IIF)
                        IT(1) = DTEMP
                        DTEMP = 100.0D0 * (DTEMP - IT(1))
                        IT(2) = DTEMP
                        DTEMP = 100.0D0 * (DTEMP - IT(2))
                        IT(3) = DTEMP + 0.1
                        CALL DAT2JD (IT, JD)
                        IF (JD0.LE.0.0D0) JD0 = JD
                        TIMES(MAXSCN) = JD - JD0 +
     *                     HEADS(SUTDA+1,1,IIF)/24.0D0
                        END IF
                     DIREC(MAXSCN,IIF) = IR
                     NREC = HEADS(SDLEN,1,IIF) + 0.1
                     NREC = ((NREC - 1) / 512 + 1) * 128
                     NVDATA = MAX (NVDATA, NREC)
                     NREC = HEADS(SNOIN,1,IIF) + 0.1
                     IF (NCHAN.EQ.0) NCHAN = NREC
                     IF (NREC.NE.NCHAN) THEN
                        WRITE (MSGTXT,1060) NREC, NCHAN
                        IRET = 8
                        GO TO 990
                        END IF
                     END IF
                  END IF
               END IF
 70         CONTINUE
 80      CONTINUE
      IF (NIF.LE.0) THEN
         MSGTXT = 'REQUESTED LINEOTF DATA NOT FOUND - USE OTFIN'
         IRET = 1
         GO TO 990
         END IF
C                                       read the directory to find the
C                                       OFFs
      IDR = 1
      DO 180 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFUVO (1, IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZBFLIP (4, 128, RBUF, RBUF)
            END IF
         DO 170 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
               IF ((FOUND(LIF).GT.0) .AND. (LS.GE.MNOFF) .AND.
     *            (LS.LE.MXOFF)) THEN
                  IIF = 0
                  DO 110 II = 1,NIF
                     IF (LIF.EQ.IFNUMB(II)) IIF = II
 110                 CONTINUE
                  IF (IIF.LE.0) THEN
                     WRITE (MSGTXT,1050) LIF
                     CALL MSGWRT (6)
                     GO TO 170
                     END IF
C                                       read the header
                  IR = IBUF(16*J-15)
                  DO 120 II = 1,3
                     JJ = NWSP * (II - 1) + 1
                     CALL OTFUVO (1, IR, IHEADS(JJ,2,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 120                 CONTINUE
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,2,IIF),
     *               HEADS(1,2,IIF))
                  CALL DL2CHR (HEADS(SOBSM,2,IIF), SWAPED, SCNTYP)
                  IR = IBUF(16*J-15)
C                                       OTF on data
                  IF (SCNTYP.EQ.'LINETPMF') THEN
                     DO 130 II = 1,MAXSCN
                        IF (OSCAN(II).EQ.LS) DIROFF(II,IIF) = IR
 130                    CONTINUE
                     END IF
                  END IF
               END IF
 170        CONTINUE
 180     CONTINUE
C                                       read the directory to find the
C                                       OFFs
C                                       read and check the bootstrap
      IDR = 1
      CALL OTFUVO (2, IDR, IBUF, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (SWAPED) CALL ZI32IL (128, 1, IBUF, IBUF)
      IF ((IBUF(3).NE.512) .OR. (IBUF(4).NE.64)) THEN
         WRITE (MSGTXT,1010) I, IBUF(3), IBUF(4)
         IRET = 1
         GO TO 990
         END IF
      MAXDIR = IBUF(5)
      IF ((IBUF(7).NE.0) .OR. (IBUF(8).NE.1)) THEN
         WRITE (MSGTXT,1011) I, IBUF(7), IBUF(8)
         CALL MSGWRT (6)
         END IF
      IDR = 1
      DO 280 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFUVO (2, IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZBFLIP (4, 128, RBUF, RBUF)
            END IF
         DO 270 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
               IF ((FOUND(LIF).GT.0) .AND. (LS.GE.MNGAIN) .AND.
     *            (LS.LE.MXGAIN)) THEN
                  IIF = 0
                  DO 210 II = 1,NIF
                     IF (LIF.EQ.IFNUMB(II)) IIF = II
 210                 CONTINUE
                  IF (IIF.LE.0) THEN
                     WRITE (MSGTXT,1050) LIF
                     CALL MSGWRT (6)
                     GO TO 270
                     END IF
C                                       read the header
                  IR = IBUF(16*J-15)
                  DO 230 II = 1,MAXSCN
                     IF (GSCAN(II).EQ.LS) DIRGN(II,IIF) = IR
 230                 CONTINUE
                  END IF
               END IF
 270        CONTINUE
 280     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVD: ERROR',I4,1X,A,'ING ',A,' FILE')
 1010 FORMAT ('OTFUFD:',I1,': RECORD, DIR LENGTHS',2I5,' NOT 512 64')
 1011 FORMAT ('OTVUVD:',I1,': Type, Version',2I5,
     *   ' not 0 1 - continuing')
 1015 FORMAT ('OTFUVD: ERROR READING ',A,' FOR SCAN',I7)
 1020 FORMAT ('Including IF',I3,' as the first IF')
 1021 FORMAT ('Including IF',I3)
 1022 FORMAT ('Excluding IF',I3,' frequency or increment do not match')
 1023 FORMAT ('F,W =',1PE14.7,1PE12.4,'   not ',1PE14.7,1PE12.4)
 1024 FORMAT ('Excluding IF',I3,' too many IFs for me')
 1050 FORMAT ('OTFUVD: SOMETHING WRONG WITH IF INDEXING AT IF',I4,
     *   ' SKIPPED')
 1060 FORMAT ('OTFUVD: NUMBER CHANNELS',I5,' DOES NOT MATCH NCHAN',I5)
      END
      SUBROUTINE OTFUVR (NV, VDATA, IRET)
C-----------------------------------------------------------------------
C   reads the next data scan, making sure that the Off and Gain data for
C   it are also read in and available.
C   Output:
C      IRET     I      Error code: 0 ok, > 0 error => die, -1 => eod
C-----------------------------------------------------------------------
      INTEGER   NV, IRET
      REAL      VDATA(NV,*)
C
      INTEGER   ISC, IR, I, J, NREC, LOFF, LGAIN, NC, MXR2, IT(6), IIF
      CHARACTER SCNTYP*8
      LOGICAL   DOWARN
      DOUBLE PRECISION DTEMP
      INCLUDE 'OTFUV.INC'
      INTEGER   GOT(MAXIFS)
      DATA DOWARN /.TRUE./
C-----------------------------------------------------------------------
      IF (CURSCN.LT.1) CURSCN = 0
      CALL FILL (3, 0, IT(4))
C                                       try to get a data scan
 10   CURSCN = CURSCN + 1
      IF (CURSCN.LE.MAXSCN) THEN
         CALL FILL (MAXIFS, 0, GOT)
C                                       Get all IFs
         DO 150 IIF = 1,NIF
C                                       there is some sort of data
            IF (DIREC(CURSCN,IIF).GT.0) THEN
C                                       read the header
               IR = DIREC(CURSCN,IIF)
               DO 20 I = 1,3
                  J = NWSP * (I - 1) + 1
                  CALL OTFUVO (1, IR, IHEADS(J,1,IIF), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) 'DATA HEADER', CURSCN
                     GO TO 990
                     END IF
                  IR = IR + 1
 20               CONTINUE
               IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,1,IIF),
     *            HEADS(1,1,IIF))
C                                       read the data
               NREC = HEADS(SDLEN,1,IIF) + 0.1
               NREC = (NREC - 1) / 512 + 1
               J = NREC * 128
               IF (J.GT.NVDATA) THEN
                  WRITE (MSGTXT,1020) 'SOURCE', J, NVDATA
                  IRET = 8
                  GO TO 990
                  END IF
               DO 30 I = 1,NREC
                  J = NWSP * (I - 1) + 1
                  CALL OTFUVO (1, IR, VDATA(J,IIF), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) 'DATA VALUES', CURSCN
                     GO TO 990
                     END IF
                  IR = IR + 1
 30               CONTINUE
               J = NREC * 128
               IF (SWAPED) CALL ZBFLIP (4, J, VDATA(1,IIF),
     *            VDATA(1,IIF))
C                                       is this data?
               CALL DL2CHR (HEADS(SOBSM,1,IIF), SWAPED, SCNTYP)
C                                       No
               IF (SCNTYP.NE.'LINEOTF ') THEN
                  WRITE (MSGTXT,1030) CURSCN, SCNTYP
                  CALL MSGWRT (6)
                  GO TO 10
                  END IF
               GOT(IIF) = GOT(IIF) + 1
C                                       have data - do we we have off
               LOFF = HEADS(SOFFS,1,IIF) + 0.001
               LGAIN = HEADS(SGAIN,1,IIF) + 0.001
               IF ((LOFF.NE.OSCAN(CURSCN)) .OR. (LGAIN.NE.GSCAN(CURSCN))
     *            .OR. (DIROFF(CURSCN,IIF).LE.0) .OR.
     *            (DIRGN(CURSCN,IIF).LE.0)) THEN
                  WRITE (MSGTXT,1035) CURSCN, LOFF, LGAIN
                  CALL MSGWRT (6)
                  GO TO 10
                  END IF
C                                       copy the off
               IF (CUROF2(IIF).EQ.LOFF) THEN
                  CALL COPY (384, HEADS(1,4,IIF), HEADS(1,2,IIF))
                  NREC = HEADS(SDLEN,2,IIF) + 0.1
                  NREC = (NREC - 1) / 512 + 1
                  J = NREC * 128
                  CALL RCOPY (J, ODATA2(1,IIF), ODATA(1,IIF))
                  CUROFF(IIF) = CUROF2(IIF)
                  TIMEO1 = TIMEO2
C                                       read from disk
               ELSE IF (CUROFF(IIF).NE.LOFF) THEN
C                                       read the header
                  IR = DIROFF(CURSCN,IIF)
                  DO 40 I = 1,3
                     J = NWSP * (I - 1) + 1
                     CALL OTFUVO (1, IR, IHEADS(J,2,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1010) 'OFF HEADER', LOFF
                        GO TO 990
                        END IF
                     IR = IR + 1
 40                  CONTINUE
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,2,IIF),
     *               HEADS(1,2,IIF))
C                                       read the data
                  NREC = HEADS(SDLEN,2,IIF) + 0.1
                  NREC = (NREC - 1) / 512 + 1
                  J = NREC * 128
                  IF (J.GT.NODATA) THEN
                     WRITE (MSGTXT,1020) 'OFF', J, NODATA
                     IRET = 8
                     GO TO 990
                     END IF
                  DO 50 I = 1,NREC
                     J = NWSP * (I - 1) + 1
                     CALL OTFUVO (1, IR, IODATA(J,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1010) 'DATA VALUES', CURSCN
                        GO TO 990
                        END IF
                     IR = IR + 1
 50                  CONTINUE
                  J = NREC * 128
                  IF (SWAPED) CALL ZBFLIP (4, J, ODATA(1,IIF),
     *               ODATA(1,IIF))
C                                       is this OFF data?
                  CALL DL2CHR (HEADS(SOBSM,2,IIF), SWAPED, SCNTYP)
                  IF (SCNTYP.NE.'LINETPMF') THEN
                     WRITE (MSGTXT,1050) LOFF, CURSCN, SCNTYP
                     CALL MSGWRT (6)
                     GO TO 10
                     END IF
                  CUROFF(IIF) = LOFF
                  DTEMP = HEADS(SUTDA,2,IIF)
                  IT(1) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(1))
                  IT(2) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(2))
                  IT(3) = DTEMP + 0.1
                  CALL DAT2JD (IT, JD)
                  TIMEO1 = JD - JD0 + HEADS(SUTDA+1,2,IIF)/24.0D0
                  END IF
               GOT(IIF) = GOT(IIF) + 1
C                                       get the gain if needed
C                                       copy the off
               IF (CURGN2(IIF).EQ.LGAIN) THEN
                  CALL COPY (384, HEADS(1,5,IIF), HEADS(1,3,IIF))
                  NREC = HEADS(SDLEN,3,IIF) + 0.1
                  NREC = (NREC - 1) / 512 + 1
                  J = NREC * 128
                  CALL RCOPY (J, GDATA2(1,IIF), GDATA(1,IIF))
                  CURGN(IIF) = CURGN2(IIF)
                  TIMEG1 = TIMEG2
C                                       read from disk
               ELSE IF (CURGN(IIF).NE.LGAIN) THEN
C                                       read the header
                  IR = DIRGN(CURSCN,IIF)
                  DO 60 I = 1,3
                     J = NWSP * (I - 1) + 1
                     CALL OTFUVO (2, IR, IHEADS(J,3,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1010) 'GAIN HEADER', LGAIN
                        GO TO 990
                        END IF
                     IR = IR + 1
 60                  CONTINUE
                  IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,3,IIF),
     *               HEADS(1,3,IIF))
C                                       read the data
                  NREC = HEADS(SDLEN,3,IIF) + 0.1
                  NREC = (NREC - 1) / 512 + 1
                  J = NREC * 128
                  IF (J.GT.NODATA) THEN
                     WRITE (MSGTXT,1020) 'GAIN', J, NODATA
                     IRET = 8
                     GO TO 990
                     END IF
                  DO 70 I = 1,NREC
                     J = NWSP * (I - 1) + 1
                     CALL OTFUVO (2, IR, IGDATA(J,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1010) 'GAIN VALUES', LGAIN
                        GO TO 990
                        END IF
                     IR = IR + 1
 70                  CONTINUE
                  J = NREC * 128
                  IF (SWAPED) CALL ZBFLIP (4, J, GDATA(1,IIF),
     *               GDATA(1,IIF))
                  CURGN(IIF) = LGAIN
                  DTEMP = HEADS(SUTDA,3,IIF)
                  IT(1) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(1))
                  IT(2) = DTEMP
                  DTEMP = 100.0D0 * (DTEMP - IT(2))
                  IT(3) = DTEMP + 0.1
                  CALL DAT2JD (IT, JD)
                  TIMEG1 = JD - JD0 + HEADS(SUTDA+1,3,IIF)/24.0D0
                  END IF
               GOT(IIF) = GOT(IIF) + 1
C                                       Find next OFF to average
               IF (DOWT.LE.0.0) GO TO 150
               DO 85 ISC = CURSCN+1,MAXSCN
                  IF ((OSCAN(ISC).NE.OSCAN(CURSCN)) .AND.
     *               (DIROFF(ISC,IIF).GT.0) .AND.
     *               (TIMES(ISC).GT.TIMES(CURSCN))) THEN
C                                       Already have
                     IF (CUROF2(IIF).EQ.OSCAN(ISC)) GO TO 90
C                                       read the header
                     IR = DIROFF(ISC,IIF)
                     DO 75 I = 1,3
                        J = NWSP * (I - 1) + 1
                        CALL OTFUVO (1, IR, IHEADS(J,4,IIF), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1010) '2ND OFF HEADER', LOFF
                           GO TO 990
                           END IF
                        IR = IR + 1
 75                     CONTINUE
                     IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,4,IIF),
     *                  HEADS(1,4,IIF))
C                                       read the data
                     NREC = HEADS(SDLEN,4,IIF) + 0.1
                     NREC = (NREC - 1) / 512 + 1
                     J = NREC * 128
                     IF (J.GT.NODATA) THEN
                        WRITE (MSGTXT,1020) 'OFF', J, NODATA
                        IRET = 8
                        GO TO 990
                        END IF
                     DO 80 I = 1,NREC
                        J = NWSP * (I - 1) + 1
                        CALL OTFUVO (1, IR, IODAT2(J,IIF), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1010) 'OFF 2 DATA', LOFF
                           GO TO 990
                           END IF
                        IR = IR + 1
 80                     CONTINUE
                     J = NREC * 128
                     IF (SWAPED) CALL ZBFLIP (4, J, ODATA2(1,IIF),
     *                  ODATA2(1,IIF))
C                                       is this OFF data?
                     CALL DL2CHR (HEADS(SOBSM,4,IIF), SWAPED, SCNTYP)
                     IF (SCNTYP.NE.'LINETPMF') THEN
                        WRITE (MSGTXT,1050) LOFF, CURSCN, SCNTYP
                        CALL MSGWRT (6)
                        GO TO 10
                        END IF
                     CUROF2(IIF) = OSCAN(ISC)
                     DTEMP = HEADS(SUTDA,4,IIF)
                     IT(1) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(1))
                     IT(2) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(2))
                     IT(3) = DTEMP + 0.1
                     CALL DAT2JD (IT, JD)
                     TIMEO2 = JD - JD0 + HEADS(SUTDA+1,4,IIF)/24.0D0
                     GO TO 90
                     END IF
 85               CONTINUE
C                                       none found
C                                       copy the off
               CALL COPY (384, HEADS(1,2,IIF), HEADS(1,4,IIF))
               NREC = HEADS(SDLEN,2,IIF) + 0.1
               NREC = (NREC - 1) / 512 + 1
               J = NREC * 128
               CALL COPY (J, ODATA(1,IIF), ODATA2(1,IIF))
               CUROF2(IIF) = CUROFF(IIF)
               TIMEO2 = TIMEO1
C                                       look for gain scan
 90            DO 105 ISC = CURSCN+1,MAXSCN
                  IF ((GSCAN(ISC).NE.GSCAN(CURSCN)) .AND.
     *               (DIRGN(ISC,IIF).GT.0) .AND.
     *               (TIMES(ISC).GT.TIMES(CURSCN))) THEN
C                                       Already have
                     IF (CURGN2(IIF).EQ.GSCAN(ISC)) GO TO 150
C                                       read the header
                     IR = DIRGN(ISC,IIF)
                     DO 95 I = 1,3
                        J = NWSP * (I - 1) + 1
                        CALL OTFUVO (2, IR, IHEADS(J,5,IIF), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1010) 'GAIN HEADER 2', LGAIN
                           GO TO 990
                           END IF
                        IR = IR + 1
 95                     CONTINUE
                     IF (SWAPED) CALL ZBFLIP (8, 192, HEADS(1,5,IIF),
     *                  HEADS(1,5,IIF))
C                                       read the data
                     NREC = HEADS(SDLEN,5,IIF) + 0.1
                     NREC = (NREC - 1) / 512 + 1
                     J = NREC * 128
                     IF (J.GT.NODATA) THEN
                        WRITE (MSGTXT,1020) 'GAIN 2', J, NODATA
                        IRET = 8
                        GO TO 990
                        END IF
                     DO 100 I = 1,NREC
                        J = NWSP * (I - 1) + 1
                        CALL OTFUVO (2, IR, IGDAT2(J,IIF), IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1010) 'GAIN VALUES', LGAIN
                           GO TO 990
                           END IF
                        IR = IR + 1
 100                    CONTINUE
                     J = NREC * 128
                     IF (SWAPED) CALL ZBFLIP (4, J, GDATA2(1,IIF),
     *                  GDATA2(1,IIF))
                     CURGN2(IIF) = GSCAN(ISC)
                     DTEMP = HEADS(SUTDA,5,IIF)
                     IT(1) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(1))
                     IT(2) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(2))
                     IT(3) = DTEMP + 0.1
                     CALL DAT2JD (IT, JD)
                     TIMEG2 = JD - JD0 + HEADS(SUTDA+1,5,IIF)/24.0D0
                     GO TO 150
                     END IF
 105              CONTINUE
C                                       none found
C                                       copy the off
               CALL COPY (384, HEADS(1,3,IIF), HEADS(1,5,IIF))
               NREC = HEADS(SDLEN,3,IIF) + 0.1
               NREC = (NREC - 1) / 512 + 1
               J = NREC * 128
               CALL COPY (J, GDATA(1,IIF), GDATA2(1,IIF))
               CURGN2(IIF) = CURGN(IIF)
               TIMEG2 = TIMEG1
C                                       Skip non data scan
            ELSE
               GO TO 10
               END IF
 150        CONTINUE
C                                       record counters
         MXR2 = HEADS(SINTT,1,1) / HEADS(SSAMP,1,1) + 0.1
         NREC = HEADS(SDLEN,1,1) + 0.1
         NC = HEADS(SNOIN,1,1) + 0.1
         MAXREC = NREC / (4 * (NC + 5))
         IF ((MXR2.NE.MAXREC) .AND. (DOWARN)) THEN
            WRITE (MSGTXT,1069) MAXREC, MXR2
            CALL MSGWRT (6)
            DOWARN = .FALSE.
            END IF
         IBCNT = MIN (IBCNT, CURSCN)
         IECNT = MAX (IECNT, CURSCN)
         IF (DOWT.LE.0.0) THEN
            WRITE (MSGTXT,1070) DSCAN(CURSCN), BIF, EIF, MAXREC,
     *         CUROFF(1), CURGN(1)
         ELSE
            WRITE (MSGTXT,1071) DSCAN(CURSCN), BIF, EIF, MAXREC,
     *         CUROFF(1), CUROF2(1), CURGN(1), CURGN2(1)
            END IF
         CALL MSGWRT (2)
         END IF
      IF (CURSCN.GT.MAXSCN) IRET = -1
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('OTFUVR: ERROR READING ',A,' FOR SCAN',I7)
 1020 FORMAT ('OTFUVR: ',A,' DATA HAS',I8,' VALUES, BUFFER ONLY',I8)
 1030 FORMAT ('OTVUFR: SCAN',I7,' ObsMode = ',A,
     *   ' not LINEOTF or LINETPMF, skipped')
 1035 FORMAT ('OTVUFR: SCAN',I7,' Off',I7,' or gain',I7,
     *   ' missing, skipped')
 1050 FORMAT ('OTVUFR: OFF SCAN',I7,' for scan',I7,' ObsMode = ',A,
     *   ' not LINETPMF, skipped')
 1069 FORMAT ('OTFUVR: MAXREC',I6,' FROM BYTES',I6,' FROM TIMES')
 1070 FORMAT ('Scan',I6,' IFs',I3,'-',I2,' samples',I5,' with off',I6,
     *   ' and gain',I6)
 1071 FORMAT ('Scan',I5,' IF',I3,'-',I2,' samples',I5,' offs',I5,'/',I5,
     *   ' gains',I5,'/',I5)
      END
      SUBROUTINE OTFUVM (FSTVIS, IRET)
C-----------------------------------------------------------------------
C   OTFUVM takes care of creating the output file, allowing and
C   preparing for concatenation if requested and appropriate.
C   Outputs:
C      FSTVIS   I   Visibility offset for writing
C      IRET     I   Error code
C-----------------------------------------------------------------------
      INTEGER   FSTVIS, IRET
C
      INTEGER   LSEQ, LVOL, XCAT(256), OKAY, I, J
      REAL      XCATR(256)
      HOLLERITH XCATH(256)
      DOUBLE PRECISION XCATD(128)
      CHARACTER LNAME*12, LCLASS*6, LTYPE*2, STAT*4, CTEMP*8
      INCLUDE 'OTFUV.INC'
      EQUIVALENCE (XCAT, XCATR, XCATH, XCATD)
C-----------------------------------------------------------------------
      FSTVIS = 0
C                                       Do concatenation?
      IF ((XDOCON.GT.0.0) .AND. (SEQOUT.GT.0)) THEN
         IF (XFQTOL.LE.0.0) XFQTOL = 2.
         XFQTOL = XFQTOL * 1.E6
         LTYPE = 'UV'
         CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAME)
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLASS)
         LSEQ = SEQOUT
         LVOL = DISKO
         CALL CATDIR ('SRCH', LVOL, CCNO, LNAME, LCLASS, LSEQ, LTYPE,
     *      NLUSER, STAT, SCRBUF, IRET)
C                                       Found it: read header
         IF (IRET.EQ.0) THEN
            NCFILE = NCFILE + 1
            DISKO = LVOL
            FVOL(NCFILE) = DISKO
            FCNO(NCFILE) = CCNO
            FRW(NCFILE) = 1
C                                       read header
            CALL CATIO ('READ', DISKO, CCNO, XCAT, 'REST', SCRBUF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READ'
               GO TO 990
               END IF
C                                       test header
            IRET = 0
            DO 10 I = 1,5
               OKAY = 0
               J = 2 * (I - 1) + KHCTP
               IF (XCATH(J).NE.CATH(J)) OKAY = I
               IF (XCATH(J+1).NE.CATH(J+1)) OKAY = I
               J = I - 1
               IF (I.LT.3) THEN
                  IF (XCATD(KDCRV+J).NE.CATD(KDCRV+J)) OKAY = I
                  IF (XCATR(KRCRP+J).NE.CATR(KRCRP+J)) OKAY = I
                  IF (XCATR(KRCIC+J).NE.CATR(KRCIC+J)) OKAY = I
               ELSE IF (I.EQ.3) THEN
                  IF (ABS(XCATD(KDCRV+J)-CATD(KDCRV+J)).GT.XFQTOL) OKAY
     *               = I
                  IF (ABS(XCATR(KRCRP+J)-CATR(KRCRP+J)).GT.0.5) OKAY = I
                  IF (ABS(XCATR(KRCIC+J)-CATR(KRCIC+J)).GT.
     *               0.03*ABS(CATR(KRCIC+J))) OKAY = I
                  END IF
               IF (XCAT(KINAX+J).NE.CATBLK(KINAX+J)) OKAY = I
               IF (OKAY.NE.0) THEN
                  IRET = 8
                  WRITE (MSGTXT,1005) I
                  CALL MSGWRT (6)
                  OKAY = 0
                  END IF
 10            CONTINUE
C                                       update status
            IF (IRET.EQ.0) THEN
               CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', SCRBUF,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'UPDAT'
                  CALL MSGWRT (8)
                  END IF
               FSTVIS = CATBLK(KIGCN)
C                                       set JD0
               CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
               CALL JULDAY (CTEMP, JD0)
               JD0 = JD0 - XADDAY
               CALL H2CHR (2, 1, CATH(KITYP), STAT)
               SORTED = STAT(:1).EQ.'T'
               END IF
            GO TO 999
C                                       error
         ELSE IF (IRET.NE.5) THEN
            WRITE (MSGTXT,1010) IRET
            GO TO 990
C                                       missing
         ELSE
            MSGTXT = 'Specified concatenation file not found' //
     *         ' making new one'
            CALL MSGWRT (6)
            END IF
         END IF
C                                       Create the UV file
      CCNO = 1
      CALL UVCREA (DISKO, CCNO, SCRBUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET, 'CREAT'
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFUVM: ERROR',I4,1X,A,'ING CONCAT UV FILE HEADER')
 1005 FORMAT ('OTFUVM: DATA AND CONCAT FILE DO NOT MATCH ON AXIS',I3)
 1010 FORMAT ('OTFUVM: ERROR',I5,' LOOKING FOR CONCATENATE FILE')
 1020 FORMAT ('OTFUVM: ERROR',I4,1X,A,'ING OUTPUT UV FILE')
      END
      SUBROUTINE OTFANT
C-----------------------------------------------------------------------
C   Make an antenna file for the output data file iff one does not
C   already exist: errors are ignored
C-----------------------------------------------------------------------
C
      INTEGER   LUNA,  IABUF(512), FINDA, I, JERR, ANVER, MAXA
      DOUBLE PRECISION JDA, GASTM
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'OTFUV.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUNA /30/
C-----------------------------------------------------------------------
C                                       Is there one already
      ANVER = 1
      CALL ISTAB ('AN', DISKO, CCNO, ANVER, LUNA, IABUF, TABLE, EXIST,
     *   FITASC, JERR)
C      IF (EXIST) GO TO 999
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
      ANAME = 'NRAO 12M'
C                                       Array center (rel to center of
C                                       earth): shift from VLBA KP one
      ARRAYC(1) = -1995945.D0
      ARRAYC(2) = -5037360.D0
      ARRAYC(3) =  3357261.D0
C                                       Get GST0 and Earth rotation rate
      CALL H2CHR (8, 1, CATH(KHDOB), RDATE)
      CALL JULDAY (RDATE, JDA)
      CALL GSTROT (JDA, GSTIA0, GASTM, DEGPDY)
      SAFREQ = CATD(KDCRV+2)
      ANFQID = -1
      ANTNIF = 1
      XYZHAN = 'RIGHT'
      DIAMAN = 12.
C                                       Create/init file
      CALL ANTINI ('WRIT', IABUF, FVOL(1), FCNO(1), ANVER, CATBLK, LUNA,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, JERR)
      IF (JERR.NE.0) GO TO 990
      MAXA = IANRNO - 1
      MAXA = MAX (MAXA, EIF)
      MAXA = MAX (MAXA, 1)
C                                       init basic AN record
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
      DO 10 I = 1,MAXA
         NOSTA = I
         WRITE (ANNAME,1000) I
         IANRNO = I
         CALL TABAN ('WRIT', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, JERR)
         IF (JERR.NE.0) GO TO 990
 10      CONTINUE
C                                       Close/update AN file.
      CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, JERR)
      GO TO 999
C                                       Error
 990  CALL ZCLOSE (LUNA, FINDA, JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('12M IF',I2.2)
      END
      SUBROUTINE DL2CHR (DT, SWAPED, STR)
C-----------------------------------------------------------------------
C   translates a double precision (local form) to character
C   Inputs:
C      DT       D     Double precision in local form
C      SWAPED   L     Are we byte swapped?
C   Output:
C      STR      C*8   Character form
C-----------------------------------------------------------------------
      DOUBLE PRECISION DT
      LOGICAL   SWAPED
      CHARACTER STR*8
C
      DOUBLE PRECISION TD
      HOLLERITH HT(2)
      EQUIVALENCE (TD, HT)
C-----------------------------------------------------------------------
      TD = DT
      IF (SWAPED) CALL ZBFLIP (8, 1, TD, TD)
      CALL H2CHR (8, 1, HT, STR)
C
 999  RETURN
      END

LOCAL INCLUDE 'OTFBS.INC'
C                                       Local include for OTFBS
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
C                                       inputs
      HOLLERITH XINFIL(12), XOUNAM(3), XOUCLS(2)
      REAL      XBCNT, XECNT, XBIF, XEIF, XOUSEQ, XOUDSK, XDOCON,
     *   XFQTOL,  XADDAY
      COMMON /INPARM/ XINFIL, XBCNT, XECNT, XBIF, XEIF, XOUNAM, XOUCLS,
     *   XOUSEQ, XOUDSK, XDOCON, XFQTOL, XADDAY
C                                       internals
      INTEGER   SCRBUF(256), SEQOUT(2), DISKO(2), BCOUNT, ECOUNT, BIF,
     *   EIF, JBUFSZ, IBCNT, IECNT, NIF, CATB1(256), CATB2(256),
     *   SDCNO(2), NCHAN, CATS(256,2)
      CHARACTER NAMOUT*12, CLAOUT(2)*6, INFILE*48, CFRAME*8
      REAL      BUFF(UVBFSS,2), COSDEC, TIMAX, THROW(2), CATR1(256),
     *   CATR2(256), TIMERR, LTIME
      LOGICAL   COMPRS, SORTED, SWAPED, DOMSG
      HOLLERITH CATH1(256), CATH2(256)
      DOUBLE PRECISION CATD1(128), CATD2(128)
      EQUIVALENCE (CATB1, CATR1, CATH1, CATD1, CATS(1,1))
      EQUIVALENCE (CATB2, CATR2, CATH2, CATD2, CATS(1,2))
      COMMON /OTFBSP/ CATS, SCRBUF, BUFF, SEQOUT, DISKO, BCOUNT, ECOUNT,
     *   NIF, BIF, EIF, JBUFSZ, COMPRS, IBCNT, IECNT, COSDEC, SORTED,
     *   SWAPED, TIMAX, DOMSG, SDCNO, NCHAN, THROW, TIMERR, LTIME
      COMMON /OTFBSC/ INFILE, NAMOUT, CLAOUT, CFRAME
C                                       SDD IO package
      INTEGER   DIRENT, NVDATA, NODATA, MAXIFS
      PARAMETER (MAXIFS = 8)
      PARAMETER (DIRENT = 32768)
      PARAMETER (NODATA = 2048)
      DOUBLE PRECISION HEADS(192,MAXIFS), JD0, JD
      INTEGER   DIREC(DIRENT,MAXIFS), IOBUF(256), ILUN, IIND, IIRET,
     *   IIREC, CURSCN, CURREC, MAXREC, IFNUMB(MAXIFS), JBCNT, JECNT,
     *   NWDP, NWSP, TYPBUF(512), TYMBUF(512), TYPLUN, TYMLUN, TYPRNO,
     *   TYMRNO, TYKOLS(MAXTYC), TYNUMV(MAXTYC), IHEADS(384,MAXIFS)
      REAL      WEIGHT(MAXIFS), VSCAL(MAXIFS), VOFF(MAXIFS),
     *   TANT(MAXIFS), TSYS(MAXIFS)
      EQUIVALENCE (IHEADS, HEADS)
      COMMON /SDDIO/ HEADS, DIREC, IOBUF, JD0, JD, ILUN, IIND,
     *   IIRET, IIREC, CURSCN, CURREC, MAXREC, WEIGHT, NVDATA, IFNUMB,
     *   VSCAL, VOFF, JBCNT, JECNT, NWDP, NWSP, TYPBUF, TYMBUF, TYPLUN,
     *   TYMLUN, TYPRNO, TYMRNO, TANT, TSYS, TYKOLS, TYNUMV
      INCLUDE 'INCS:DSDD.INC'
C                                       standard includes
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
LOCAL END
      PROGRAM OTFBS
C-----------------------------------------------------------------------
C! Translates on-the-fly continuum beam-switch SDD format to UV files
C# Task General Singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1996-1997, 1999, 2009, 2015
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
C   Task to read singledish beam-switched continuum data in UniPOPS
C   internal format (SDD) and write 2 AIPS UV data files in the
C   single-dish format.
C   Limitations:
C      Now only does OTF beam-switch continuum data from the 12m
C   Input adverbs:
C      INFILE......Input 12m sdd file (raw data file).
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
C                  tasks, OTFBS has to increase the size of the UV
C                  file after creation if there is more data than it
C                  guessed initially.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'OTFBS.INC'
      DATA PRGM /'OTFBS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL OTFBSI (PRGM, IRET)
C                                       write the file
      CALL OTFBSF (IRET)
C                                       fill in history file
      CALL OTFBSH (IRET)
C                                       Close down files, etc.
      CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE OTFBSI (PRGM, IRET)
C-----------------------------------------------------------------------
C   gets input parameters for OTFBS, file creation has to wait
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
      INTEGER   NPARM, IERR, IROUND
      INCLUDE 'OTFBS.INC'
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      SWAPED = .FALSE.
      DOMSG = .TRUE.
      CALL RFILL (MAXIFS, 0.0, TANT)
      CALL RFILL (MAXIFS, 0.0, TSYS)
      CALL RFILL (MAXIFS, 0.0, VOFF)
      CALL RFILL (MAXIFS, 1.0, VSCAL)
      NWSP = 4096 / NBITWD
      NWDP = NWSP / NWDPDP
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 26
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
      SEQOUT(1) = IROUND (XOUSEQ)
      SEQOUT(2) = SEQOUT(1)
      DISKO(1) = IROUND (XOUDSK)
      DISKO(2) = DISKO(1)
      BCOUNT = IROUND (XBCNT)
      IF (BCOUNT.LE.0) BCOUNT = 1
      ECOUNT = IROUND (XECNT)
      IF (ECOUNT.LE.0) ECOUNT = BCOUNT + DIRENT - 2
      IF (ECOUNT-BCOUNT.GE.DIRENT) THEN
         ECOUNT = BCOUNT + DIRENT - 2
         WRITE (MSGTXT,1010) ECOUNT
         CALL MSGWRT (6)
         END IF
      IBCNT = ECOUNT
      IECNT = BCOUNT
      JBCNT = ECOUNT - BCOUNT + 1
      JECNT = 1
      BIF = IROUND (XBIF)
      IF ((BIF.LT.1) .OR. (BIF.GT.99)) BIF = 1
      EIF = IROUND (XEIF)
      IF ((EIF.LT.BIF) .OR. (EIF.GT.99)) EIF = 99
C                                       No compression - dynamic range
C                                       problems with degrees K and
C                                       coordinates
      COMPRS = .FALSE.
      SORTED = .TRUE.
      TIMAX = -1.0
      TIMERR = 1000.
      LTIME = -1000.
C                                       Characters
      CALL H2CHR (12, 1, XOUNAM, NAMOUT)
      CALL H2CHR (6, 1, XOUCLS, CLAOUT(1))
      CALL H2CHR (48, 1, XINFIL, INFILE)
      XADDAY = IROUND (XADDAY)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFBSI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('OTFBSI warning: ECOUNT reduced to',I6,' to fit')
      END
      SUBROUTINE OTFBSF (IRET)
C-----------------------------------------------------------------------
C   Reads the input SDD data, creates and writes the output UV file
C   In/Out:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   NIOUT(2), NUMVIS, XCOUNT(2), IPTRO(2), NIOLIM, LUNO(2),
     *   INDO(2), NCOPY, NCORO, BO, KBIND(2), ILENBU, NREC, CRAZY,
     *   FSTVIS, I, IIF, IP, ID, MIF
      CHARACTER PHFILE(2)*48
      LOGICAL   T, F
      INCLUDE 'OTFBS.INC'
      INTEGER   MXVDAT
      PARAMETER (MXVDAT = 9000)
      INTEGER   IVDATA(MXVDAT,MAXIFS)
      REAL      TBUFF(36*MAXIFS), VDATA(MXVDAT,MAXIFS), CTIME,
     *   RPARMS(8,4*MAXIFS)
      EQUIVALENCE (IVDATA, VDATA)
      DATA LUNO, BO /16, 17, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Init the SDD IO
      NUMVIS = 0
      ILUN = 18
      TYPLUN = 19
      TYMLUN = 20
      TYPRNO = -1
      TYMRNO = -1
      IIREC = -1
      CURSCN = -1
      CURREC = -1
      MAXREC = -1
      I = DIRENT * MAXIFS
      CALL FILL (I, 0, DIREC)
      NVDATA = 0
      CALL OTFBSD (IRET)
      IF (IRET.NE.0) GO TO 999
      NCHAN = 3
      IF (NVDATA.GT.MXVDAT) THEN
         WRITE (MSGTXT,1000) NVDATA, MXVDAT
         CALL MSGWRT (8)
         IRET = 8
         GO TO 999
         END IF
      NVDATA = MXVDAT
C                                       Initial call - make UV header
      CALL OTFBSG (NUMVIS, RPARMS, NCHAN, TBUFF, NVDATA, VDATA, IRET)
      IF (IRET.GT.0) GO TO 999
C                                       create the file or decide on
C                                       catcatenation
      CALL OTFBSM (FSTVIS, IRET)
      IF (IRET.GT.0) GO TO 999
      XCOUNT(1) = FSTVIS
      XCOUNT(2) = FSTVIS
C                                       make an antenna file
C                                       do not die on error
      CALL OTFANT
C                                       save output names
      SEQOUT(1) = CATB1(KIIMS)
      CALL H2CHR (12, KHIMNO, CATH1(KHIMN), NAMOUT)
      CALL H2CHR (6, KHIMCO, CATH1(KHIMC), CLAOUT(1))
      SEQOUT(2) = CATB2(KIIMS)
      CALL H2CHR (6, KHIMCO, CATH2(KHIMC), CLAOUT(2))
C                                       interpret the header
      CALL COPY (256, CATB1, CATBLK)
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Open vis file for write
      DO 10 ID = 1,2
         CALL ZPHFIL ('UV', DISKO(ID), SDCNO(ID), 1, PHFILE(ID), IRET)
         CALL ZOPEN (LUNO(ID), INDO(ID), DISKO(ID), PHFILE(ID), T, F, F,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'OPEN', ID
            GO TO 990
            END IF
 10      CONTINUE
C                                       Get last time
      IF ((SORTED) .AND. (FSTVIS.GT.0)) THEN
         ILENBU = 0
         I = FSTVIS - 1
         CALL UVINIT ('READ', LUNO(1), INDO(1), 1, I, LREC, ILENBU,
     *      JBUFSZ, BUFF(1,1), BO, KBIND(1), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'INIT', 1
            GO TO 990
            END IF
         CALL UVDISK ('READ', LUNO(1), INDO(1), BUFF(1,1), NIOUT(1),
     *      KBIND(1), IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1010) IRET, 'READ', 1
            GO TO 990
            END IF
         TIMAX = BUFF(KBIND(1)+2,1)
         END IF
C                                       Init vis file for write
      DO 20 ID = 1,2
         ILENBU = 0
         CRAZY = 4000000 + FSTVIS
         CALL UVINIT ('WRIT', LUNO(ID), INDO(ID), CRAZY, FSTVIS, LREC,
     *      ILENBU, JBUFSZ, BUFF(1,ID), BO, KBIND(ID), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'INIT', ID
            GO TO 990
            END IF
         IPTRO(ID) = KBIND(ID)
         NIOUT(ID) = 0
 20      CONTINUE
      NIOLIM = ILENBU
      NCORO = (LREC - NRPARM) / CATB1(KINAX)
      NCOPY = LREC - NRPARM
      MIF = 4 * NIF
C                                       Loop
 100  CONTINUE
C                                       Get next sample
         NUMVIS = NUMVIS + 1
         CALL OTFBSG (NUMVIS, RPARMS, NCHAN, TBUFF, NVDATA, VDATA, IRET)
         IF (IRET.GT.0) GO TO 999
C                                       Got datum
         IF (IRET.EQ.0) THEN
            DO 150 IIF = 1,MIF
               ID = (IIF - 1) / NIF
               ID = MOD (ID, 2) + 1
               CALL RCOPY (6, RPARMS(1,IIF), BUFF(IPTRO(ID),ID))
               IP = 3 * NCHAN * (IIF - 1) + 1
               IF (COMPRS) THEN
                  CALL ZUVPAK (NCORO, TBUFF(IP), BUFF(IPTRO(ID)+6,ID),
     *               BUFF(IPTRO(ID)+NRPARM,ID))
               ELSE
                  CALL RCOPY (NCOPY, TBUFF(IP),
     *               BUFF(IPTRO(ID)+NRPARM,ID))
                  END IF
               XCOUNT(ID) = XCOUNT(ID) + 1
               CTIME = BUFF(IPTRO(ID)+2,ID)
               IF (CTIME-TIMAX.LE.-2.E-7) THEN
                  SORTED = .FALSE.
                  TIMERR = MIN (TIMERR, CTIME-TIMAX)
                  END IF
               TIMAX = MAX (CTIME, TIMAX)
               IPTRO(ID) = IPTRO(ID) + LREC
               NIOUT(ID) = NIOUT(ID) + 1
C                                       Expand file
               IF (XCOUNT(ID).GT.CATS(KIGCN,ID)) THEN
                  NREC = (10100 * LREC * 2) / 512 + 1
                  CALL ZEXPND (LUNO(ID), DISKO(ID), PHFILE(ID), NREC,
     *               IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1010) IRET, 'EXPAND', ID
                     GO TO 990
                     END IF
                  CALL ZEXIST (DISKO(ID), PHFILE(ID), NREC, IRET)
                  CATS(KIGCN,ID) = (NREC * 256.0D0) / LREC
                  END IF
C                                       Write vis record.
               IF (NIOUT(ID).GE.NIOLIM) THEN
                  CALL UVDISK ('WRIT', LUNO(ID), INDO(ID), BUFF(1,ID),
     *               NIOUT(ID), KBIND(ID), IRET)
                  IF (IRET.GT.0) THEN
                     WRITE (MSGTXT,1010) IRET, 'WRIT', ID
                     GO TO 990
                     END IF
                  IPTRO(ID) = KBIND(ID)
                  NIOUT(ID) = 0
                  END IF
 150           CONTINUE
C                                       Next vis.
            GO TO 100
            END IF
C                                       Finish write
      DO 160 ID = 1,2
         NIOUT(ID) = - NIOUT(ID)
         CALL UVDISK ('FLSH', LUNO(ID), INDO(ID), BUFF(1,ID), NIOUT(ID),
     *      KBIND(ID), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1010) IRET, 'FLSH', ID
            GO TO 990
            END IF
C                                       Compress output file.
         NVIS = XCOUNT(ID)
         CALL UCMPRS (NVIS, DISKO(ID), SDCNO(ID), LUNO(ID), CATS(1,ID),
     *      IRET)
C                                       Close file
         CALL ZCLOSE (LUNO(ID), INDO(ID), IRET)
 160     CONTINUE
      IRET = 0
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFBSF: MAX SCAN',I6,' TOO LONG FOR BUFFER',I6)
 1010 FORMAT ('OTFBSF: ERROR',I4,1X,A,'ING OUTPUT UV FILE',I2)
      END
      SUBROUTINE OTFBSH (IRET)
C-----------------------------------------------------------------------
C   Adds history file and info to the output UV file
C   In/Out:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNH, I, BUFFH(256), IERR, ITRIM, DATE(3), TIME(3), ID
      CHARACTER LINE*72, ATIME*8, ADATE*12
      INCLUDE 'OTFBS.INC'
      DATA LUNH /28/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Write History.
      CALL HIINIT (3)
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Loop over output files
      DO 100 ID = 1,2
C                                       Create/open hist. file.
         CALL HICREA (LUNH, DISKO(ID), SDCNO(ID), CATS(1,ID), BUFFH,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (6)
            GO TO 999
            END IF
C                                       Write first record.
         WRITE (LINE,1010) TSKNAM, NLUSER, ADATE, ATIME
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       AIPS release
         WRITE (LINE,1011) TSKNAM, RLSNAM
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       output names
         CALL HENCOO (TSKNAM, NAMOUT, CLAOUT(ID), SEQOUT(ID), DISKO(ID),
     *      LUNH, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
C                                       inputs
         I = ITRIM (INFILE)
         WRITE (LINE,1020) TSKNAM, INFILE(:I)
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1025) TSKNAM, IBCNT
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
         WRITE (LINE,1026) TSKNAM, IECNT
         CALL HIADD (LUNH, LINE, BUFFH, IERR)
         IF (IERR.NE.0) GO TO 90
         DO 20 I = 1,NIF
            WRITE (LINE,1027) TSKNAM, IFNUMB(I)
            CALL HIADD (LUNH, LINE, BUFFH, IERR)
            IF (IERR.NE.0) GO TO 90
 20         CONTINUE
C                                       Close HI file
 90      CALL HICLOS (LUNH, .TRUE., BUFFH, IERR)
C                                       Close TY files
         IF (ID.EQ.1) THEN
            CALL TABIO ('CLOS', 0, TYPRNO, TYPBUF, TYPBUF, IERR)
         ELSE
            CALL TABIO ('CLOS', 0, TYMRNO, TYMBUF, TYMBUF, IERR)
            END IF
C                                       Update header
         IF (SORTED) THEN
            MSGTXT = 'Data are in time order'
            IF (ID.EQ.1) CALL MSGWRT (2)
            CALL CHR2H (2, 'T ', 1, CATS(KITYP,ID))
         ELSE
            MSGTXT = 'WARNING: DATA ARE NOT IN TIME ORDER'
            IF (ID.EQ.1) CALL MSGWRT (6)
            TIMERR = -86400 * TIMERR
            WRITE (MSGTXT,1100) TIMERR
            IF (ID.EQ.1) CALL MSGWRT (6)
            CALL CHR2H (2, '  ', 1, CATS(KITYP,ID))
            END IF
         CALL CATIO ('UPDT', DISKO(ID), SDCNO(ID), CATS(1,ID), 'REST',
     *      SCRBUF, IERR)
 100     CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFBSH: ERROR',I3,' CREATE/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'/ AIPS Release = ''',A7,' ''')
 1020 FORMAT (A6,'INFILE =''',A,''' / data')
 1025 FORMAT (A6,'BCOUNT =',I6,5X,'/ first scan to include')
 1026 FORMAT (A6,'ECOUNT =',I6,5X,'/ last scan to include')
 1027 FORMAT (A6,'IF =',I3,12X,'/  IF number included')
 1100 FORMAT ('MAXIMUM TIME MISORDER =',F11.4,' SECONDS')
      END
      SUBROUTINE OTFBSO (NR, REC, IRET)
C-----------------------------------------------------------------------
C   does actual read IO from disk file
C   Inputs:
C      NR     I        Desired record number (512-byte records)
C   Outputs:
C      REC    I(128)   Data record
C      IRET   I        I/O error: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   NR, REC(128), IRET
C
      INTEGER   IP, IR
      CHARACTER FILET*4
      INCLUDE 'OTFBS.INC'
      DATA FILET /'DATA'/
C-----------------------------------------------------------------------
      IRET = -1
C                                       ZFIO record number
      IF (NR.GT.0) THEN
         IR = (NR-1)/2 + 1
C                                       don't have it yet
         IF (IR.NE.IIREC) THEN
            IIREC = IR
            CALL ZFIO ('READ', ILUN, IIND, IIREC, IOBUF, IIRET)
            IRET = IIRET
            IF ((IRET.NE.0) .AND. (IRET.LT.1128)) THEN
               WRITE (MSGTXT,1000) IRET, FILET, NR
               GO TO 990
               END IF
            END IF
C                                       copy the data
         IP = 1 + 128 * MOD (NR-1, 2)
         CALL COPY (128, IOBUF(IP), REC)
         IRET = 0
         IF ((IP.GT.1) .AND. (IIRET.GT.1000) .AND.
     *      (IIRET.LT.1256)) THEN
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
 1000 FORMAT ('OTFBSO: ERROR',I5,' READING SDD ',A,' FILE RECORD',I8)
 1010 FORMAT ('OTFBSO: ATTEMPT TO READ PAST END-OF-FILE RECORD',I8)
      END
      SUBROUTINE OTFBSG (NUMVIS, RPARMS, NC, VIS, NV, VDATA, IRET)
C-----------------------------------------------------------------------
C   Data reader and interpreter for OTFBS
C   Inputs:
C      NUMVIS   I       0 => fill in CATBLK, using 1st scan, open inputs
C                      >0 => return 1 Vis record
C                      <0 => close down
C   Outputs:
C      RPARMS   R(*)   Random parameters (except weight and scale)
C      VIS      R(*)   Data (Flux, 0, weight) by nchan by +-+-
C   In/Out:
C      IRET     I      0 => ok, > 0 => die, < 0 => out of data
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, NC, NV, IRET
      REAL      VDATA(NV,*), RPARMS(8,*), VIS(3,NC,*)
C
      INTEGER   I, J, NRP, IT(6), IP, K, IIF, ID, IOFF, LIF, VER,
     *   NUMPOL, NUMIF
      REAL      SS(5), TEMP, TA, TS
      CHARACTER CTEMP*8, RTYPE(8)*8, CTYPE(5)*8
      DOUBLE PRECISION DTEMP
      INCLUDE 'OTFBS.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA RTYPE /'AZ---REL', 'EL---REL', 'TIME1', 'BEAM', 'SCAN',
     *   'SAMPLE', 'WEIGHT', 'SCALE'/
      DATA CTYPE /'COMPLEX', 'STOKES', 'FREQ', 'RA', 'DEC'/
      DATA IT /6*0/
C-----------------------------------------------------------------------
      IF (IRET.NE.0) GO TO 999
C                                       Initial call
      IF (NUMVIS.EQ.0) THEN
C                                       Get first data
         CALL OTFBSR (NV, VDATA, IRET)
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
         DTEMP = HEADS(SUTDA,1)
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
C                                       Throws
         CALL DL2CHR (HEADS(SFRAM,1), SWAPED, CFRAME)
         THROW(1) = HEADS(SPBEM,1)
         THROW(2) = HEADS(SMBEM,1)
C                                       Telescope.
         CALL DL2CHR (HEADS(STELE,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHTEL))
C                                       Receiver
         CALL DL2CHR (HEADS(SFRNT,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHINS))
C                                       Object.
         CALL DL2CHR (HEADS(SOBJE,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHOBJ))
C                                       Observer's name.
         CALL DL2CHR (HEADS(SOBSI,1), SWAPED, CTEMP)
         CALL CHR2H (8, CTEMP, 1, CATH(KHOBS))
C                                       Epoch.
         CATR(KREPO) = HEADS(SEPOC,1)
C                                       "Old" (observed) position.
         CATD(KDORA) = HEADS(SXSOU,1)
         CATD(KDODE) = HEADS(SYSOU,1)
         COSDEC = COS (CATD(KDODE) * DG2RAD)
         IF (COSDEC.EQ.0.0) COSDEC = 1.0
C                                       Rest Frequency
         CATD(KDRST) = HEADS(SRSTF,1) * 1.0D6
C                                       Alternate ref. value & pixel
         CATD(KDARV) = HEADS(SX0,1) * 1.0D3
         CATR(KRARP) = HEADS(SREFP,1)
         CALL DL2CHR (HEADS(SVELD,1), SWAPED, CTEMP)
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
         CATD(KDCRV+2) = HEADS(SOBSF,1) * 1.0D6
         CATBLK(KINAX+2) = NCHAN
         CATR(KRCIC+2) = HEADS(SFRQR,1) * 1.0D6
         CATR(KRCRP+2) = HEADS(SREFP,1)
C                                       Convolution size: 1.2 lambda/D
         CATR(KRBMJ) = RAD2DG * VELITE / 1.0D1 / CATD(KDCRV+2)
         CATR(KRBMN) = CATR(KRBMJ)
C                                       Image name parameters
         IF (NAMOUT.EQ.' ') CALL H2CHR (8, 1, CATH(KHOBJ), NAMOUT)
         CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
         CALL CHR2H (2, 'UV', KHPTYO, CATR(KHPTY))
         CALL COPY (256, CATBLK, CATB1)
         CALL COPY (256, CATBLK, CATB2)
         CATB1(KIIMS) = SEQOUT(1)
         CATB2(KIIMS) = SEQOUT(2)
         IF (CLAOUT(1).EQ.' ') CLAOUT(1) = TSKNAM
         CLAOUT(2) = CLAOUT(1)
         CLAOUT(2)(6:6) = '-'
         CLAOUT(1)(6:6) = '+'
         CALL CHR2H (6, CLAOUT(1), KHIMCO, CATH1(KHIMC))
         CALL CHR2H (6, CLAOUT(2), KHIMCO, CATH2(KHIMC))
C                                       Later calls for data
      ELSE IF (NUMVIS.GT.0) THEN
C                                       Need more data
         CURREC = MAX (0, CURREC+1)
         IF (CURREC.GE.MAXREC) THEN
            CALL OTFBSR (NV, VDATA, IRET)
            IF (IRET.NE.0) GO TO 999
            CURREC = 0
            END IF
         IF (CURREC.EQ.0) THEN

            DTEMP = HEADS(SUTDA,1)
            IT(1) = DTEMP
            DTEMP = 100.0D0 * (DTEMP - IT(1))
            IT(2) = DTEMP
            DTEMP = 100.0D0 * (DTEMP - IT(2))
            IT(3) = DTEMP + 0.1
            IT(1) = IT(1) - 1900
            CALL FILL (3, 0, IT(4))
            CALL DAT2JD (IT, JD)
            DO 115 IIF = 1,NIF
C                                       Test coordinate
               IF (DOMSG) THEN
                  IF ((ABS(CATD1(KDORA)-HEADS(SXSOU,IIF)).GE.1.0) .OR.
     *               (ABS(CATD1(KDODE)-HEADS(SYSOU,IIF)).GE.1.0)) THEN
                     DOMSG = .FALSE.
                     WRITE (MSGTXT,1100) 'RA ', CATD1(KDORA),
     *                  HEADS(SXSOU,IIF)
                     CALL MSGWRT (7)
                     WRITE (MSGTXT,1100) 'DEC', CATD1(KDODE),
     *                  HEADS(SYSOU,IIF)
                     CALL MSGWRT (7)
                     END IF
                  END IF
C                                       Planets -> use first coord
               IF (CFRAME(:5).EQ.'EPHEM') THEN
                  HEADS(SXSOU,IIF) = CATD1(KDORA)
                  HEADS(SYSOU,IIF) = CATD1(KDODE)
                  END IF
C                                       Counts to Kelvins
               TEMP = 0.0
               IF (HEADS(SAPPE,IIF).GT.0.0) TEMP = HEADS(SETAA,IIF) *
     *            HEADS(SETAF,IIF) / HEADS(SAPPE,IIF)
               IF (TEMP.LE.0.0) TEMP = 1.0
               VSCAL(IIF) = HEADS(SRTSY,IIF) *  HEADS(SETAA,IIF) *
     *            HEADS(SETAF,IIF)
               IF (VSCAL(IIF).LE.0.0) VSCAL(IIF) = 1.0
C                                       Noise based weight
               TS = HEADS(STSYS,IIF)
               TA = TS * TEMP
               WEIGHT(IIF) =  TS / 1000.0D0
               IF (WEIGHT(IIF).LE.0.0) WEIGHT(IIF) = 1.0
               IF (HEADS(SSAMP,IIF).GT.0.0D0) THEN
                  WEIGHT(IIF) = (10.0 * HEADS(SSAMP,IIF)) /
     *               (WEIGHT(IIF) * WEIGHT(IIF))
               ELSE
                  WEIGHT(IIF) = 1.0 / (WEIGHT(IIF) * WEIGHT(IIF))
                  END IF
C                                       TY table
               IF ((TS.NE.TSYS(IIF)) .OR. (TA.NE.TANT(IIF))) THEN
                  VER = 1
                  NUMPOL = 1
                  NUMIF = 1
                  IF (TYPRNO.LT.0) THEN
                     CALL TYINI ('WRIT', TYPBUF, DISKO(1), SDCNO(1),
     *                  VER, CATB1, TYPLUN, TYPRNO, TYKOLS, TYNUMV,
     *                  NUMPOL, NUMIF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     CALL TYINI ('WRIT', TYMBUF, DISKO(2), SDCNO(2),
     *                  VER, CATB2, TYMLUN, TYMRNO, TYKOLS, TYNUMV,
     *                  NUMPOL, NUMIF, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
                  TSYS(IIF) = TS
                  TANT(IIF) = TA
                  IP = 4 * MAXREC + CURREC + 1
                  TEMP = VDATA(IP,IIF)/86.4D6 + JD - JD0
                  CALL TABTY ('WRIT', TYPBUF, TYPRNO, TYKOLS, TYNUMV,
     *               NUMPOL, NUMIF, TEMP, 0.0, 0, IIF, 0, 0, TSYS(IIF),
     *               TANT(IIF), IRET)
                  IF (IRET.NE.0) GO TO 999
                  CALL TABTY ('WRIT', TYMBUF, TYMRNO, TYKOLS, TYNUMV,
     *               NUMPOL, NUMIF, TEMP, 0.0, 0, IIF, 0, 0, TSYS(IIF),
     *               TANT(IIF), IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
 115           CONTINUE
            END IF
         DO 160 ID = 1,4
            DO 150 IIF = 1,NIF
               LIF = IIF + (ID - 1) * NIF
               IOFF = -1
               IF (CURREC.EQ.0) IOFF = 1
               IF ((ID.GT.2) .AND. (CURREC.LT.MAXREC-1)) IOFF = 1
               IP = 4 * MAXREC + CURREC + 1
               DO 120 K = 1,5
                  SS(K) = VDATA(IP,IIF)
                  SS(K) = SS(K) + (5. - 2.*ID) * IOFF / 8.0 *
     *               (SS(K) - VDATA(IP+IOFF,IIF))
                  IP = IP + MAXREC
 120              CONTINUE
               RPARMS(3,LIF) = JD - JD0 + SS(1)/86.4D6
               IF (LTIME.EQ.-1000.) LTIME = RPARMS(3,IIF)
C                                       day changed
               IF (LTIME-RPARMS(3,IIF).GT.0.8) THEN
                  JD = JD + 1.0D0
                  RPARMS(3,IIF) = RPARMS(3,IIF) + 1.0
                  END IF
               LTIME = RPARMS(3,IIF)
               SS(2) = SS(2) / COSDEC
C                                       az, el offset from
C                                       center ra, dec
               RPARMS(1,LIF) = SS(4)
               RPARMS(2,LIF) = SS(5)
C                                       one-beam system
               RPARMS(4,LIF) = 257.0 * IFNUMB(IIF)
               IP = HEADS(SSCAN,IIF) + 0.01
               RPARMS(5,LIF) = IP
               RPARMS(6,LIF) = CURREC + 1
               IP = 4 * CURREC + ID
               IF (VDATA(IP,IIF).NE.1.0E-20) THEN
                  IF (VOFF(IIF).EQ.0.0) VOFF(IIF) = VDATA(IP,IIF) *
     *               VSCAL(IIF)
                  VIS(1,1,LIF) = VDATA(IP,IIF)*VSCAL(IIF) - VOFF(IIF)
                  VIS(2,1,LIF) = VOFF(IIF)
                  VIS(3,1,LIF) = WEIGHT(IIF)
               ELSE
                  VIS(1,1,LIF) = 0.0
                  VIS(1,1,LIF) = 0.0
                  VIS(3,1,LIF) = -WEIGHT(IIF)
                  END IF
C                                       RA, Dec as chans 2,3
               VIS(1,2,LIF) = SS(2)
               VIS(1,3,LIF) = SS(3)
               VIS(2,2,LIF) = 0.
               VIS(2,3,LIF) = 0.
               VIS(3,2,LIF) = VIS(3,1,LIF)
               VIS(3,3,LIF) = VIS(3,1,LIF)
 150           CONTINUE
 160        CONTINUE
C                                       Final call
      ELSE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (I4.4,2I2.2)
 1100 FORMAT ('PROBABLE ERROR: MERGING ',A,' COORDINATES',2F12.7)
      END
      SUBROUTINE OTFBSD (IRET)
C-----------------------------------------------------------------------
C   opens the data file and reads in its directory for the specified
C   range in BCOUNT, ECOUNT, and IF = BIF-EIF.
C   Output:
C      IRET     I     Error code: 0 okay, else die
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   J, IBUF(128), MAXDIR, ID, IR, LS, II, JJ, ISC, NREC,
     *   IT(6), IDR, LIF, IIF, FOUND(100)
      DOUBLE PRECISION DTEMP, FREQS(100), FREQB
      LOGICAL   T, F, FIRST
      REAL      RBUF(128), RTEMP, INCRS(100), INCRB
      CHARACTER INF*48, FTYPE*4, SCNTYP*8
      INCLUDE 'OTFBS.INC'
      DATA FTYPE /'DATA'/
      DATA T,F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      CALL FILL (100, 0, FOUND)
      FIRST = .TRUE.
C                                       open data and gains files
      INF = INFILE
      CALL ZOPEN (ILUN, IIND, 1, INF, F, F, T, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN', FTYPE
         GO TO 990
         END IF
      SWAPED = .FALSE.
      CALL FILL (3, 0, IT(4))
C                                       build up the directory
C                                       read and check the bootstrap
      IDR = 1
      CALL OTFBSO (IDR, IBUF, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Swapped bytes ??
      IF ((IBUF(3).NE.512) .AND. (IBUF(4).NE.64)) THEN
         CALL ZI32IL (128, 1, IBUF, IBUF)
         SWAPED = (IBUF(3).EQ.512) .AND. (IBUF(4).EQ.64)
         IF (.NOT.SWAPED) CALL ZILI32 (128, IBUF, 1, IBUF)
         END IF
      IF ((IBUF(3).NE.512) .OR. (IBUF(4).NE.64)) THEN
         WRITE (MSGTXT,1010) IBUF(3), IBUF(4)
         IRET = 1
         GO TO 990
         END IF
      MAXDIR = IBUF(5)
      IF ((IBUF(7).NE.0) .OR. (IBUF(8).NE.1)) THEN
         WRITE (MSGTXT,1011) IBUF(7), IBUF(8)
         CALL MSGWRT (6)
         END IF
C                                       read directory, set IFs
      IDR = 1
      DO 30 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFBSO (IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZR32RL (128, 1, RBUF, RBUF)
            END IF
         DO 25 J = 1,8
            IF (ID+J-1.LE.MAXDIR) THEN
               RTEMP = RBUF(9+16*(J-1))
               LS = RTEMP + 0.001
               LIF = 100.0 * (RTEMP - LS) + 0.1
               IF ((LIF.GE.BIF) .AND. (LIF.LE.EIF) .AND. (LS.GE.BCOUNT)
     *            .AND. (LS.LE.ECOUNT) .AND. (FOUND(LIF).EQ.0)) THEN
C                                       read the header
                  IR = IBUF(16*J-15)
                  DO 20 II = 1,3
                     JJ = NWSP * (II - 1) + 1
                     CALL OTFBSO (IR, IHEADS(JJ,1), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 20                  CONTINUE
                  IF (FIRST) CALL SDDPTS (HEADS)
                  FIRST = .FALSE.
                  IF (SWAPED) CALL ZR64RL (192, 1, HEADS(1,1),
     *               HEADS(1,1))
                  CALL DL2CHR (HEADS(SOBSM,1), SWAPED, SCNTYP)
C                                       OTF continuum data
                  IF (SCNTYP.EQ.'CONTOTF ') THEN
                     FOUND(LIF) = 1
                     FREQS(LIF) = HEADS(SOBSF,1)
                     INCRS(LIF) = HEADS(SFRQR,1)
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
                  WRITE (MSGTXT,1023) FREQS(LIF), INCRS(LIF),
     *               FREQB, INCRB
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
      DO 80 ID = 1,MAXDIR,8
         IDR = IDR + 1
         CALL OTFBSO (IDR, IBUF, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL COPY (NWSP, IBUF, RBUF)
         IF (SWAPED) THEN
            CALL ZI32IL (128, 1, IBUF, IBUF)
            CALL ZR32RL (128, 1, RBUF, RBUF)
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
C                                       off or on
C                                       read the header
                  ISC = LS-BCOUNT+1
                  IR = IBUF(16*J-15)
                  DO 60 II = 1,3
                     JJ = NWSP * (II - 1) + 1
                     CALL OTFBSO (IR, IHEADS(JJ,IIF), IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1015) 'DATA HEADER', ISC
                        GO TO 990
                        END IF
                     IR = IR + 1
 60                  CONTINUE
                  IF (SWAPED) CALL ZR64RL (192, 1, HEADS(1,IIF),
     *               HEADS(1,IIF))
                  CALL DL2CHR (HEADS(SOBSM,IIF), SWAPED, SCNTYP)
                  IR = IBUF(16*J-15)
C                                       OTF continuum data
                  IF (SCNTYP.EQ.'CONTOTF ') THEN
                     DIREC(ISC,IIF) = IR
                     JBCNT = MIN (ISC, JBCNT)
                     JECNT = MAX (ISC, JECNT)
                     NREC = HEADS(SDLEN,IIF) + 0.1
                     NREC = (NREC + 3) / 4
                     NVDATA = MAX (NVDATA, NREC)
                     NREC = HEADS(SNOIN,IIF) + 0.1
                     IF (NCHAN.EQ.0) NCHAN = NREC
                     IF (NREC.NE.NCHAN) THEN
                        WRITE (MSGTXT,1060) NREC, NCHAN
                        IRET = 8
                        GO TO 990
                        END IF
                     END IF
C                                       Observing date.
C                                       get time
                  IF ((DIREC(ISC,IIF).NE.0) .AND. (JD0.LE.0.0D0)) THEN
                     DTEMP = HEADS(SUTDA,IIF)
                     IT(1) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(1))
                     IT(2) = DTEMP
                     DTEMP = 100.0D0 * (DTEMP - IT(2))
                     IT(3) = DTEMP + 0.1
                     IT(1) = IT(1) - 1900
                     CALL DAT2JD (IT, JD)
                     IF (JD0.LE.0.0D0) JD0 = JD
                     END IF
                  END IF
               END IF
 70         CONTINUE
 80      CONTINUE
      JBCNT = JBCNT + BCOUNT - 1
      JECNT = JECNT + BCOUNT - 1
      IF (NIF.LE.0) THEN
         MSGTXT = 'REQUESTED CONTOTF DATA NOT FOUND - USE OTFIN'
         CALL MSGWRT (8)
         IRET = 1
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFBSD: ERROR',I4,1X,A,'ING ',A,' FILE')
 1010 FORMAT ('OTFBSD: RECORD, DIR LENGTHS',2I5,' NOT 512 64')
 1011 FORMAT ('OTVBSD: Type, Version',2I5,' not 0 1 - continuing')
 1015 FORMAT ('OTFBSD: ERROR READING ',A,' FOR SCAN',I7)
 1020 FORMAT ('Including IF',I3,' as the first IF')
 1021 FORMAT ('Including IF',I3)
 1022 FORMAT ('Excluding IF',I3,' frequency or increment do not match')
 1023 FORMAT ('F,W =',1PE14.7,1PE12.4,'   not ',1PE14.7,1PE12.4)
 1024 FORMAT ('Excluding IF',I3,' too many IFs for me')
 1050 FORMAT ('OTFBSD: SOMETHING WRONG WITH IF INDEXING AT IF',I4,
     *   ' SKIPPED')
 1060 FORMAT ('OTFBSD: NUMBER CHANNELS',I5,' DOES NOT MATCH NCHAN',I5)
      END
      SUBROUTINE OTFBSR (NV, VDATA, IRET)
C-----------------------------------------------------------------------
C   reads the next data scan, making sure that the Off and Gain data for
C   it are also read in and available.
C   Output:
C      IRET     I      Error code: 0 ok, > 0 error => die, -1 => eod
C-----------------------------------------------------------------------
      INTEGER   NV, IRET
      REAL      VDATA(NV,*)
C
      INTEGER   ISC, IR, I, J, NREC, IT(6), IIF
      CHARACTER SCNTYP*8
      INCLUDE 'OTFBS.INC'
      INTEGER   GOT(MAXIFS)
C-----------------------------------------------------------------------
      IF (CURSCN.LT.BCOUNT) CURSCN = BCOUNT - 1
      IF (CURSCN.LT.JBCNT) CURSCN = JBCNT - 1
      CALL FILL (3, 0, IT(4))
C                                       try to get a data scan
 10   CURSCN = CURSCN + 1
C                                       end of data
      IF (CURSCN.GT.JECNT) THEN
         IRET = -1
C                                       get data
      ELSE
         CALL FILL (MAXIFS, 0, GOT)
C                                       Get all IFs
         DO 50 IIF = 1,NIF
            ISC = CURSCN - BCOUNT + 1
C                                       there is some sort of data
            IF (DIREC(ISC,IIF).GT.0) THEN
C                                       read the header
               IR = DIREC(ISC,IIF)
               DO 20 I = 1,3
                  J = NWSP * (I - 1) + 1
                  CALL OTFBSO (IR, IHEADS(J,IIF), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) 'DATA HEADER', CURSCN
                     GO TO 990
                     END IF
                  IR = IR + 1
 20               CONTINUE
               IF (SWAPED) CALL ZR64RL (192, 1, HEADS(1,IIF),
     *            HEADS(1,IIF))
C                                       read the data
               NREC = HEADS(SDLEN,IIF) + 0.1
               NREC = (NREC - 1) / 512 + 1
               J = NREC * 128
               IF (J.GT.NVDATA) THEN
                  WRITE (MSGTXT,1020) 'SOURCE', J, NVDATA
                  IRET = 8
                  GO TO 990
                  END IF
               DO 30 I = 1,NREC
                  J = NWSP * (I - 1) + 1
                  CALL OTFBSO (IR, VDATA(J,IIF), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1010) 'DATA VALUES', CURSCN
                     GO TO 990
                     END IF
                  IR = IR + 1
 30               CONTINUE
               J = NREC * 128
               IF (SWAPED) CALL ZR32RL (J, 1, VDATA(1,IIF),
     *            VDATA(1,IIF))
C                                       is this data?
               CALL DL2CHR (HEADS(SOBSM,IIF), SWAPED, SCNTYP)
C                                       bad type
               IF (SCNTYP.NE.'CONTOTF ') THEN
                  WRITE (MSGTXT,1030) CURSCN, SCNTYP
                  CALL MSGWRT (6)
                  GO TO 50
                  END IF
               GOT(IIF) = GOT(IIF) + 1
               END IF
 50         CONTINUE
C                                       did we find anything
         DO 60 IIF = 1,NIF
            IF (GOT(IIF).GT.0) GO TO 70
 60         CONTINUE
         GO TO 10
C                                       record counters
 70      NREC = HEADS(SDLEN,1) + 0.1
         MAXREC = NREC / (4 * 9)
         IBCNT = MIN (IBCNT, CURSCN)
         IECNT = MAX (IECNT, CURSCN)
         WRITE (MSGTXT,1070) CURSCN, BIF, EIF, MAXREC
         CALL MSGWRT (2)
         END IF
      IF (CURSCN.GT.ECOUNT) IRET = -1
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('OTFBSR: ERROR READING ',A,' FOR SCAN',I7)
 1020 FORMAT ('OTFBSR: ',A,' DATA HAS',I8,' VALUES, BUFFER ONLY',I8)
 1030 FORMAT ('OTVUFR: SCAN',I7,' ObsMode = ',A,' not CONTOTF, skipped')
 1070 FORMAT ('Processing scan',I7,' IFs',I3,'-',I2,' samples',I5)
      END
      SUBROUTINE OTFBSM (FSTVIS, IRET)
C-----------------------------------------------------------------------
C   OTFBSM takes care of creating the output file, allowing and
C   preparing for concatenation if requested and appropriate.
C   Outputs:
C      FSTVIS   I   Visibility offset for writing
C      IRET     I   Error code
C-----------------------------------------------------------------------
      INTEGER   FSTVIS, IRET
C
      INTEGER   LSEQ, LVOL, XCAT(256), OKAY, I, J, ID, NUMKEY, LOCS,
     *   KEYTYP
      REAL      XCATR(256)
      HOLLERITH XCATH(256)
      DOUBLE PRECISION XCATD(128)
      CHARACTER LNAME*12, LCLASS*6, LTYPE*2, STAT*4, CTEMP*8, KEYWRD*8
      INCLUDE 'OTFBS.INC'
      EQUIVALENCE (XCAT, XCATR, XCATH, XCATD)
      DATA NUMKEY, KEYWRD, LOCS, KEYTYP /1, 'BSTHROW', 1, 2/
C-----------------------------------------------------------------------
      FSTVIS = 0
C                                       Do concatenation?
      IF ((XDOCON.GT.0.0) .AND. (SEQOUT(1).GT.0)) THEN
         IF (XFQTOL.LE.0.0) XFQTOL = 2.
         XFQTOL = XFQTOL * 1.E6
         LTYPE = 'UV'
         DO 50 ID = 1,2
            CALL COPY (256, CATS(1,ID), CATBLK)
            CALL H2CHR (12, KHIMNO, CATH(KHIMN), LNAME)
            CALL H2CHR (6, KHIMCO, CATH(KHIMC), LCLASS)
            LSEQ = SEQOUT(ID)
            LVOL = DISKO(ID)
            CALL CATDIR ('SRCH', LVOL, SDCNO(ID), LNAME, LCLASS, LSEQ,
     *         LTYPE, NLUSER, STAT, SCRBUF, IRET)
C                                       Found it: read header
            IF (IRET.EQ.0) THEN
               NCFILE = NCFILE + 1
               DISKO(ID) = LVOL
               FVOL(NCFILE) = LVOL
               FCNO(NCFILE) = SDCNO(ID)
               FRW(NCFILE) = 1
C                                       read header
               CALL CATIO ('READ', LVOL, SDCNO(ID), XCAT, 'REST',
     *            SCRBUF, IRET)
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
                     IF (ABS(XCATD(KDCRV+J)-CATD(KDCRV+J)).GT.XFQTOL)
     *                  OKAY = I
                     IF (ABS(XCATR(KRCRP+J)-CATR(KRCRP+J)).GT.0.5)
     *                  OKAY = I
                     IF (ABS(XCATR(KRCIC+J)-CATR(KRCIC+J)).GT.
     *                  0.03*ABS(CATR(KRCIC+J))) OKAY = I
                     END IF
                  IF (XCAT(KINAX+J).NE.CATBLK(KINAX+J)) OKAY = I
                  IF (OKAY.NE.0) THEN
                     IRET = 8
                     WRITE (MSGTXT,1005) I
                     CALL MSGWRT (6)
                     OKAY = 0
                     END IF
 10               CONTINUE
C                                       update status
               IF (IRET.EQ.0) THEN
                  CALL CATIO ('READ', LVOL, SDCNO(ID), CATBLK, 'WRIT',
     *               SCRBUF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'UPDAT'
                     CALL MSGWRT (8)
                     END IF
                  CALL COPY (256, CATBLK, CATS(1,ID))
                  IF (ID.EQ.1) THEN
                     FSTVIS = CATBLK(KIGCN)
C                                       set JD0
                     CALL H2CHR (8, 1, CATH(KHDOB), CTEMP)
                     CALL JULDAY (CTEMP, JD0)
                     JD0 = JD0 - XADDAY
                     CALL H2CHR (2, 1, CATH(KITYP), STAT)
                     SORTED = STAT(:1).EQ.'T'
                  ELSE IF (FSTVIS.NE.CATBLK(KIGCN)) THEN
                     WRITE (MSGTXT,1010) CATBLK(KIGCN), FSTVIS
                     IRET = 6
                     GO TO 990
                     END IF
                  END IF
               IF (IRET.NE.0) GO TO 999
C                                       error
            ELSE IF (IRET.NE.5) THEN
               WRITE (MSGTXT,1010) IRET
               GO TO 990
C                                       missing
            ELSE IF (ID.EQ.2) THEN
               MSGTXT = 'SPECIFIED 2ND CONCATENATION FILE NOT FOUND'
               IRET = 6
               GO TO 990
C                                       missing
            ELSE
               MSGTXT = 'Specified concatenation file not found' //
     *            ' making new one'
               CALL MSGWRT (6)
               GO TO 100
               END IF
 50         CONTINUE
         GO TO 999
         END IF
C                                       Create the UV file
 100  DO 120 ID = 1,2
         SDCNO(ID) = 1
         CALL COPY (256, CATS(1,ID), CATBLK)
         CALL UVCREA (DISKO(ID), SDCNO(ID), SCRBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, 'CREAT'
            GO TO 990
            END IF
         NCFILE = NCFILE + 1
         FVOL(NCFILE) = DISKO(ID)
         FCNO(NCFILE) = SDCNO(ID)
         FRW(NCFILE) = 2
C                                       Throw into catblk
         CALL CATKEY ('WRIT', DISKO(ID), SDCNO(ID), KEYWRD, NUMKEY,
     *      LOCS, THROW(3-ID), KEYTYP, SCRBUF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1020) IRET, 'ADD THROW KEYWORD'
            GO TO 990
            END IF
         CALL COPY (256, CATBLK, CATS(1,ID))
 120     CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('OTFBSM: ERROR',I4,1X,A,'ING CONCAT UV FILE HEADER')
 1005 FORMAT ('OTFBSM: DATA AND CONCAT FILE DO NOT MATCH ON AXIS',I3)
 1010 FORMAT ('OTFBSM: ERROR',I5,' LOOKING FOR CONCATENATE FILE')
 1020 FORMAT ('OTFBSM: ERROR',I4,1X,A,'ING OUTPUT UV FILE')
      END
      SUBROUTINE OTFANT
C-----------------------------------------------------------------------
C   Make an antenna file for the output data file iff one does not
C   already exist: errors are ignored
C-----------------------------------------------------------------------
C
      INTEGER   LUNA, IABUF(512), FINDA, I, JERR, ANVER, MAXA, ID
      DOUBLE PRECISION JDA, GASTM
      LOGICAL   TABLE, EXIST, FITASC
      INCLUDE 'OTFBS.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      DATA LUNA /30/
C-----------------------------------------------------------------------
C                                       Is there one already
      DO 100 ID = 1,2
         CALL COPY (256, CATS(1,ID), CATBLK)
         ANVER = 1
         CALL ISTAB ('AN', DISKO(ID), SDCNO(ID), ANVER, LUNA, IABUF,
     *      TABLE, EXIST, FITASC, JERR)
C                                       Setup for AN table initization
         NUMORB = 0
         NOPCAL = 2
         ANTNIF = 1
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
C                                       Create/init file
         XYZHAN = 'RIGHT'
         CALL ANTINI ('WRIT', IABUF, DISKO(ID), SDCNO(ID), ANVER,
     *      CATBLK, LUNA, IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0,
     *      DEGPDY, SAFREQ, RDATE, POLRXY, UT1UTC, DATUTC, TIMSYS,
     *      ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL, ANTNIF, ANFQID, JERR)
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
         DIAMAN = 12.
         DO 10 I = 1,MAXA
            NOSTA = I
            WRITE (ANNAME,1000) I
            IANRNO = I
            CALL TABAN ('WRIT', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *         STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *         POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, JERR)
            IF (JERR.NE.0) GO TO 990
 10         CONTINUE
C                                       Close/update AN file.
         CALL TABIO ('CLOS', 1, IANRNO, IABUF, IABUF, JERR)
         CALL COPY (256, CATBLK, CATS(1,ID))
 100     CONTINUE
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
      IF (SWAPED) CALL ZRLR64 (1, 1, TD, TD)
      CALL H2CHR (8, 1, HT, STR)
C
 999  RETURN
      END

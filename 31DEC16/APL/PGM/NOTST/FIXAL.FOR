LOCAL INCLUDE 'FIXAL.INC'
C                                       Local include for FIXAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCSOUR(4,30),
     *   XNAMI2(3), XCLAI2(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XFLAG, XSIN2, XDISI2, CHANS(2), XSOUT,
     *   XDISO, XBCHAN, XECHAN, XCHSEL(4,20), DPARM(10), MFLUX, RFLUX
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS)
      INTEGER   SEQIN, SEQIN2, SEQOUT, DISKIN, DISKI2, DISKO, JBUFSZ,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, CHNSEL(3,20,MAXIF), NCHSEL,
     *   NIF, NFREQ, LBCHAN, LECHAN, OLDCNO, CNO2, NEWCNO,
     *   NFLUXF, CAT2(256), SOUNUM, CALNUM, NSNUMS, NCNUMS, SNUMS(500),
     *   CNUMS(30), CHMASK(MAXCIF,2), CHN1, CHN2, SCRBUF(512)
      LOGICAL   ISCOMP, DOFLUX
      CHARACTER NAMEIN*12, CLAIN*6, NAMEI2*12, CLAI2*6, NAMOUT*12,
     *   CLAOUT*6, CSOR(30)*16, SSOR(30)*16
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XCSOUR,
     *   XFLAG, XNAMI2, XCLAI2, XSIN2, XDISI2, CHANS, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XBCHAN, XECHAN, XCHSEL, DPARM, MFLUX, RFLUX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMEI2, CLAI2, NAMOUT, CLAOUT,
     *   SSOR, CSOR
      COMMON /BUFRS/ BUFF1, BUFF2, BUFF3, JBUFSZ, SCRBUF
      COMMON /INFO/ CATOLD, CAT2, SEQIN, SEQIN2, SEQOUT, DISKIN, DISKI2,
     *   DISKO, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, CHNSEL, NCHSEL, DOFLUX, NIF,
     *   NFREQ, LBCHAN, LECHAN, OLDCNO, CNO2, NEWCNO,NFLUXF, SOUNUM,
     *   CALNUM, NSNUMS, NCNUMS, SNUMS, CNUMS, CHMASK, CHN1, CHN2
LOCAL END
LOCAL INCLUDE 'FIXALBP.INC'
      REAL      BPFUNC(512,30,30,2,2,2), TIMES(3)
      COMMON /BPDATA/ TIMES, BPFUNC
LOCAL END
      PROGRAM FIXAL
C-----------------------------------------------------------------------
C! Fits aliasing function to data and undoes the aliasing part
C# UV VLA SPECTRAL
C-----------------------------------------------------------------------
C;  Copyright (C) 2008, 2011-2013, 2015
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
C   FIXAL averages a set of channels and subtracts them from another
C   range of channels.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C         calibration adverbs added
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      BCHAN          BCHAN         Start channel to write out
C      ECHAN          ECHAN         End channel to write out
C      CHANSEL(3,10)  CHNSEL        Channels to select for baseline
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'FIXAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FIXAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FIXALI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Filter data.
      CALL FIXALU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL FIXALH
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE FIXALI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FIXALI gets input parameters for FIXAL and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      ISCOMP  L  If true data is compressed
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INCLUDE 'FIXAL.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I, J, K, K1, K2, BUFFER(512),
     *   KOLS(MAXSUC), NUMV(MAXSUC), IRNO, LOOP, VER, NUMREC, IOFF
      LOGICAL   T
      INTEGER   NW(MAXIF), LUN
      REAL      CATR(256), RPARM(20)
      HOLLERITH CATH(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSOU.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK /' '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 358
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, SCRBUF, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMI2, NAMEI2)
      CALL H2CHR (6, 1, XCLAI2, CLAI2)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQIN2 = IROUND (XSIN2)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKI2 = IROUND (XDISI2)
      DISKO = IROUND (XDISO)
      CHN1 = CHANS(1) + 0.1
      CHN2 = CHANS(2) + 0.1
      SOUNUM = 0
      CALNUM = 0
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SSOR(I))
         IF (SSOR(I).NE.' ') SOUNUM = I
         CALL H2CHR (16, 1, XCSOUR(1,I), CSOR(I))
         IF (CSOR(I).NE.' ') CALNUM = I
 10      CONTINUE
      DOFLUX = (MFLUX.GT.0.0) .OR. (RFLUX.GT.0.0)
      IF (MFLUX.LE.0.0) MFLUX = 1.E20
      IF (RFLUX.LE.0.0) RFLUX = 1.E20
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      SUBARR = 1
      FGVER = IROUND (XFLAG)
C                                       Get CATBLK of alias function
      CNO2 = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKI2, CNO2, NAMEI2, CLAI2, SEQIN2, PTYPE,
     *   NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
        WRITE (MSGTXT,1030) IERR, NAMEI2, CLAI2, SEQIN2, DISKI2,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKI2, CNO2, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CAT2)
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
        WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = 1
         EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NIF = EIF - BIF + 1
      NFREQ = CATBLK(KINAX+JLOCF)
      LBCHAN = IROUND (XBCHAN)
      LECHAN = IROUND (XECHAN)
      BCHAN = 1
      ECHAN = NFREQ
C
      IF ((LBCHAN.LE.0) .OR. (LBCHAN.GT.NFREQ)) LBCHAN = 1 + (NFREQ+1)/8
      IF ((LECHAN.LE.0) .OR. (LECHAN.GT.NFREQ)) LECHAN = NFREQ -
     *   (NFREQ+1)/8
      IF (LBCHAN.GT.LECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
         END IF
      FRQSEL = 1
C                                       Set fit windows
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 20 J = 1,20
         K = IROUND (XCHSEL(2,J))
         IF (K.LE.0) GO TO 25
         K = IROUND (XCHSEL(4,J))
         IF ((K.LE.0) .OR. (K.GT.NIF)) THEN
            K1 = 1
            K2 = NIF
         ELSE
            K1 = K
            K2 = K
            END IF
         DO 15 K = K1,K2
            NW(K) = NW(K) + 1
            CHNSEL(1,NW(K),K) = MAX (0, IROUND (XCHSEL(1,J)))
            CHNSEL(2,NW(K),K) = MAX (0, IROUND (XCHSEL(2,J)))
            CHNSEL(3,NW(K),K) = MAX (1, IROUND (XCHSEL(3,J)))
 15         CONTINUE
 20      CONTINUE
C                                       If no channel selection
C                                       use 1 - NFREQ
 25   CALL FILL (2*MAXCIF, 0, CHMASK)
      DO 35 K = 1,NIF
         IOFF = (K - 1) * NFREQ
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = 1
            CHNSEL(2,1,K) = NFREQ
            CHNSEL(3,1,K) = 1
            END IF
         CALL FILL (LECHAN-LBCHAN+1, 1, CHMASK(LBCHAN+IOFF,2))
         DO 30 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NFREQ))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K)) CHNSEL(2,I,K) = NFREQ
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NFREQ))
            DO 28 J = CHNSEL(1,I,K),CHNSEL(2,I,K),CHNSEL(3,I,K)
               CHMASK(J+IOFF,1) = 1
 28            CONTINUE
 30         CONTINUE
 35      CONTINUE
C                                       for alias correction need AN, SU
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATBLK, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'GETANT ERROR FOR ALIASING CORRECTION'
         GO TO 990
         END IF
C                                       source fluxes
      IF (SOUNUM.EQ.1) THEN
         IF (SOURCS(1)(:1).EQ.'-') SOUNUM = 0
         END IF
      VER = 1
      LUN = 28
      CALL SOUINI ('READ', BUFFER, DISKIN, OLDCNO, VER, CATBLK, LUN,
     *   NUMIF, VELTYP, VELDEF, SUFQID, IRNO, KOLS, NUMV, JERR)
      IF (JERR.NE.0) THEN
         MSGTXT = 'SOUINI ERROR FOR ALIASING CORRECTION'
         GO TO 990
         END IF
      NUMREC = BUFFER(5)
      NSNUMS = 0
      NCNUMS = 0
      DO 50 LOOP = 1,NUMREC
         IRNO = LOOP
         CALL TABSOU ('READ', BUFFER, IRNO, KOLS, NUMV, IDSOUR,
     *      SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'TABSOU ERROR FOR ALIASING CORRECTION'
            GO TO 990
            END IF
         DO 40 I = 1,CALNUM
            IF (SNAME.EQ.CSOR(I)) THEN
               NCNUMS = NCNUMS + 1
               CNUMS(NCNUMS) = IDSOUR
               GO TO 50
               END IF
 40         CONTINUE
         DO 45 I = 1,SOUNUM
            IF (SNAME.EQ.SSOR(I)) THEN
               NSNUMS = NSNUMS + 1
               SNUMS(NSNUMS) = IDSOUR
               GO TO 50
               END IF
 45         CONTINUE
         IF (SOUNUM.EQ.0) THEN
            NSNUMS = NSNUMS + 1
            SNUMS(NSNUMS) = IDSOUR
            END IF
 50      CONTINUE
      CALL TABIO ('CLOS', 0, IRNO, BUFFER, BUFFER, IERR)
C                                       Fill BP table
      CALL FIXALB (.TRUE., JERR)
      IF (JERR.NE.0) GO TO 999
      CALL COPY (256, CATOLD, CATBLK)
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         JERR = 10
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      NRPRMI = NRPARM
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       read compressed => write compr.
      IF (ISCOMP) THEN
         CATBLK(KINAX) = 1
         I = CATBLK(KIPCN)
         CALL CHR2H (8, 'WEIGHT  ', 1, CATBLK(KHPTP+2*I))
         CALL CHR2H (8, 'SCALE   ', 1, CATBLK(KHPTP+2*I+2))
         CATBLK(KIPCN) = I + 2
         ILOCWT = I
         END IF
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, SCRBUF, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', SCRBUF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
      NEWCNO = CCNO
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
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', SCRBUF, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXALI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('FIXALI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE FIXALU (IRET)
C-----------------------------------------------------------------------
C   FIXALU sends uv data one point at a time to the filtering routine
C   and then writes the modified data if requested.
C   Input in common:
C      BCHAN   I  Lowest channel number to write.
C      ECHAN   I  Highest channel number to write.
C      LRECO   I  Output file record length
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, IA1,
     *   IA2, BO, VO, NUMVIS,XCOUNT, NCORO, RNXRET, NCOPY, CATMP(256),
     *   NBAD
      LOGICAL   T, F
      INCLUDE 'FIXAL.INC'
      REAL      BASEN, VIS(UVBFSS), RESULT(UVBFSS), RPARM(20)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (BUFF1, VIS)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO)
      IF (.NOT.ISCOMP) NCORO = NCORO / 3
      NCOPY = LRECO - NRPRMO
      NBAD = 0
C                                       Open and init for read
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
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
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            IA1 = BASEN / 256. + 0.1
            IA2 = BASEN - IA1*256. + 0.1
         ELSE
            IA1 = RPARM(1+ILOCA1) + 0.1
            IA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         NUMVIS = NUMVIS + 1
C                                       Call filtering routine.
         CALL FIXALF (NUMVIS, IA1, IA2, VIS, RPARM, RESULT, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            NBAD = NBAD + 1
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET, XCOUNT
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       last call message
      NUMVIS = -1
      CALL FIXALF (NUMVIS, IA1, IA2, VIS, RPARM, RESULT, IRET)
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET, XCOUNT
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, NEWCNO, LUNO, CATBLK, IRET)
C                                       close NX table
      CALL RNXCLS (RNXRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      IF (NBAD.GT.0) THEN
         WRITE (MSGTXT,1200) NBAD
         CALL MSGWRT (6)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXALU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1010 FORMAT ('FIXALU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('FIXALU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('FIXALU: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('FIXALU: ERROR',I3,' WRITING VIS FILE AT VIS',I9)
 1200 FORMAT ('DELETED',I8,' VISIBILITIES - NOT ENOUGH VALID CHANNELS',
     *   ' TO FIT')
      END
      SUBROUTINE FIXALH
C-----------------------------------------------------------------------
C   FIXALH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, K
      LOGICAL   T
      INCLUDE 'FIXAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRBUF, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
C                                       BCHAN,ECHAN
      WRITE (HILINE,2000) TSKNAM, LBCHAN, LECHAN
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      CHANSEL
      DO 25 K = 1,NIF
         DO 20 I = 1,20
            IF ((CHNSEL(1,I,K).GT.0) .AND.
     *         (CHNSEL(2,I,K).GE.CHNSEL(1,I,K))) THEN
               WRITE (HILINE,2020) TSKNAM, I, CHNSEL(1,I,K),
     *            CHNSEL(2,I,K), CHNSEL(3,I,K), K
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 20         CONTINUE
 25      CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'FIXALH: ERROR COPYING TABLES TO LINE UV'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF, IERR)
C
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXALH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/')
 2000 FORMAT (A6,'BCHAN =',I4,', ECHAN =',I4,
     *   ' / Output channel selection')
 2020 FORMAT (A6,'CHANSEL(',I2,')=',I5,',',I5,',',I2,5X,
     *   '/ channels fit IF =',I3)
      END
      SUBROUTINE FIXALF (NUMVIS, IA1, IA2, VIS, RPARM, RESULT, IRET)
C-----------------------------------------------------------------------
C   Routine to fit baseline to channels, subtract and perhaps flag
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(3,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      CHNSEL(3,10) Gives channel selection
C      INCSI   I    Input Stokes' increment in vis.
C      INCFI   I    Input frequency increment in vis.
C      INCIFI  I    Input IF increment in vis.
C      LRECO   I    Output file record length
C      NRPRMO  I    Output number of random parameters.
C      INCSO   I    Output Stokes' increment in vis.
C      INCFO   I    Output frequency increment in vis.
C      INCIFO  I    Output IF increment in vis.
C   Output:
C      RESULT  R(3,*) Output visibilities selected in frequency.
C      RESUL2  R(3,*) Baseline fit value at CHFIT0
C      IRET    I  Return code   1 => don't write - not could fit
C                               0 => OK
C   Output in COMMON:
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, IRET
      REAL      VIS(3,*), RPARM(*), RESULT(3,*)
C
      INTEGER   NPOLN, INDEX, OFF, IS, IIF, INDEX2, ICHAN, NSX, IOFF,
     *   LT, ISOU, I
      REAL      SX, SSX, MSX, BR1, VSR, VSI, SRR, SRI, WT1, WT2, W1, W2,
     *   TIME, SRD, SID, SA, SB, SC, DEN, BI1
      COMPLEX   VS, SR
      LOGICAL   FLGWT, WASOK, SOMEOK
      INCLUDE 'FIXAL.INC'
      INCLUDE 'FIXALBP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DANS.INC'
C-----------------------------------------------------------------------
C                                       Set up on first call
      IF (NUMVIS.EQ.1) NFLUXF = 0
      NPOLN = CATBLK(KINAX+JLOCS)
      IRET = 0
      SOMEOK = .FALSE.
      IF (NUMVIS.GT.0) THEN
C                                       is this an acceptable source
         LT = 1
         IF (ILOCSU.GE.0) THEN
            ISOU = RPARM(1+ILOCSU)
C                                       calibrator?
            LT = 2
            DO 10 I = 1,NCNUMS
               IF (ISOU.EQ.CNUMS(I)) GO TO 20
 10            CONTINUE
            LT = 1
            DO 15 I = 1,NSNUMS
               IF (ISOU.EQ.SNUMS(I)) GO TO 20
 15            CONTINUE
            LT = 0
            END IF
 20      IF (STNNAM(IA1)(:4).NE.'EVLA') LT = 0
         IF (STNNAM(IA2)(:4).NE.'EVLA') LT = 0
C                                       just copy
         IF (LT.EQ.0) THEN
            DO 40 IIF = 1,NIF
               DO 35 IS = 1,NPOLN
                  OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI + 1
                  INDEX = OFF
                  INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
                  DO 30 ICHAN = 1,NFREQ
                     RESULT(1,INDEX2) = VIS(1,INDEX)
                     RESULT(2,INDEX2) = VIS(2,INDEX)
                     RESULT(3,INDEX2) = VIS(3,INDEX)
                     IF (RESULT(3,INDEX2).GT.0.0) SOMEOK = .TRUE.
                     INDEX = INDEX + INCFI
                     INDEX2 = INDEX2 + INCFO
 30                  CONTINUE
 35               CONTINUE
 40            CONTINUE
C                                       EVLA-EVLA fit the thing
         ELSE
            TIME = RPARM(1+ILOCT)
            IF ((TIME.GT.TIMES(2)) .AND. (TIMES(2).GT.0.0) .AND.
     *         (TIMES(3).GT.0.0)) THEN
               CALL FIXALB (.FALSE., IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
            IF (TIMES(2).LT.0.0) THEN
               WT1 = 1.0
               WT2 = 0.0
            ELSE IF (TIME.GT.TIMES(2)) THEN
               WT2 = 1.0
               WT1 = 0.0
            ELSE
               WT1 = (TIMES(2) - TIME) / (TIMES(2) - TIMES(1))
               WT1 = MAX (0.0, MIN (1.0, WT1))
               WT2 = 1.0 - WT1
               END IF
C                                       Loop over IF
            DO 140 IIF = 1,NIF
               IOFF = (IIF-1) * NFREQ
C                                       Loop over Stokes
               DO 135 IS = 1,NPOLN
C                                       Offset in Stokes and IF
                  OFF = (IS - 1) * INCSI + (IIF - 1) * INCIFI + 1
C                                       move to array
                  INDEX = OFF
                  SR = CMPLX (0.0, 0.0)
                  SRR = 0.0
                  SRI = 0.0
                  NSX = 0
                  SRD = 0.0
                  SID = 0.0
                  SA = 0.0
                  SB = 0.0
                  SC = 0.0
                  DO 110 ICHAN = 1,NFREQ
                     IF ((VIS(3,INDEX).GT.0) .AND.
     *                  (CHMASK(ICHAN+IOFF,LT).GT.0)) THEN
                        W1 = 0.0
                        W2 = 0.0
                        IF (BPFUNC(ICHAN,IA1,IA2,IS,IIF,1).NE.FBLANK)
     *                     W1 = WT1
                        IF (BPFUNC(ICHAN,IA1,IA2,IS,IIF,2).NE.FBLANK)
     *                     W2 = WT2
                        IF ((W1.GT.0.0) .OR. (W2.GT.0.0)) THEN
                           W1 = W1 / (W1 + W2)
                           W2 = W2 / (W1 + W2)
                           VSR = VIS(1,INDEX)
                           VSI = VIS(2,INDEX)
                           VS = CMPLX (VSR, VSI)
                           BR1 = W1*BPFUNC(ICHAN,IA1,IA2,IS,IIF,1) +
     *                        W2*BPFUNC(ICHAN,IA1,IA2,IS,IIF,2)
                           BI1 = W1*BPFUNC(ICHAN,IA2,IA1,IS,IIF,1) +
     *                        W2*BPFUNC(ICHAN,IA2,IA1,IS,IIF,2)
                           SRD = SRD + VSR
                           SID = SID + VSI
                           SA = SA + (1.0 + BR1)
                           SB = SB + (1.0 - BR1)
                           SC = SC + BI1
C                          SRR = SRR + VSR / (1.0 + BR1)
C                          SRI = SRI + VSI / (1.0 - BR1)
C                          SRR = SRR + VSR * (1.0 + BR1)
C                          SRD = SRD + (1.0 + BR1)**2
C                          SRI = SRI + VSI * (1.0 - BR1)
C                          SID = SID + (1.0 - BR1)**2
                           NSX = NSX + 1
                           END IF
                        END IF
                     INDEX = INDEX + INCFI
 110                 CONTINUE
                  WASOK = NSX.GT.0
                  IF (WASOK) THEN
C                    SRI = SRI / SID
C                    SRR = SRR / SRD
C                    SRI = SRI / NSX
C                    SRR = SRR / NSX
                     DEN = SA * SB - SC * SC
                     IF (DEN.NE.0.0) THEN
                        SRR = (SRD * SB - SID * SC) / DEN
                        SRI = (SID * SA - SRD * SC) / DEN
                     ELSE
                        SRR = 0.0
                        SRI = 0.0
                        END IF
                     END IF
C                                       Subtract baseline from input
C                                       if could get a fit
                  INDEX = OFF
                  INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
                  SSX = 0.0
                  MSX = 0.0
                  NSX = 0
                  FLGWT = .FALSE.
                  DO 120 ICHAN = 1,NFREQ
C                                       Subtract from vis.
                     IF (.NOT.WASOK) VIS(3,INDEX) = -ABS (VIS(3,INDEX))
                     IF (VIS(3,INDEX).GT.0) THEN
                        W1 = 0.0
                        W2 = 0.0
                        IF (BPFUNC(ICHAN,IA1,IA2,IS,IIF,1).NE.FBLANK)
     *                     W1 = WT1
                        IF (BPFUNC(ICHAN,IA1,IA2,IS,IIF,2).NE.FBLANK)
     *                     W2 = WT2
                        IF ((W1.GT.0.0) .OR. (W2.GT.0.0)) THEN
                           W1 = W1 / (W1 + W2)
                           W2 = W2 / (W1 + W2)
                           BR1 = W1*BPFUNC(ICHAN,IA1,IA2,IS,IIF,1) +
     *                        W2*BPFUNC(ICHAN,IA1,IA2,IS,IIF,2)
                           BI1 = W1*BPFUNC(ICHAN,IA2,IA1,IS,IIF,1) +
     *                        W2*BPFUNC(ICHAN,IA2,IA1,IS,IIF,2)
                           RESULT(1,INDEX2) = VIS(1,INDEX)
     *                        - BR1 * SRR - BI1 * SRI
                           RESULT(2,INDEX2) = VIS(2,INDEX)
     *                        + BR1 * SRI - BI1 * SRR
                           RESULT(3,INDEX2) = VIS(3,INDEX)
                        ELSE
                           RESULT(1,INDEX2) = VIS(1,INDEX)
                           RESULT(2,INDEX2) = VIS(2,INDEX)
                           RESULT(3,INDEX2) = -ABS(VIS(3,INDEX))
                           END IF
                     ELSE
                        RESULT(1,INDEX2) = VIS(1,INDEX)
                        RESULT(2,INDEX2) = VIS(2,INDEX)
                        RESULT(3,INDEX2) = -ABS(VIS(3,INDEX))
                        END IF
                     IF ((DOFLUX) .AND. (RESULT(3,INDEX2).GT.0.0) .AND.
     *                  (CHMASK(ICHAN+IOFF,LT).GT.0)) THEN
                        SX = (RESULT(1,INDEX2) - REAL(SR)) ** 2 +
     *                     (RESULT(2,INDEX2) - AIMAG(SR)) ** 2
                        SSX = SSX + SX
                        MSX = MAX (MSX, SX)
                        NSX = NSX + 1
                        END IF
                     INDEX = INDEX + INCFI
                     INDEX2 = INDEX2 + INCFO
 120                 CONTINUE
                  IF (NSX.GT.0) THEN
                     SSX = SQRT (SSX / NSX)
                     MSX = SQRT (MSX)
                     IF ((SSX.GT.RFLUX) .OR. (MSX.GT.MFLUX)) THEN
                        NFLUXF = NFLUXF + 1
                        INDEX2 = (IS - 1) * INCSO + (IIF - 1) * INCIFO
     *                     + 1
                        DO 125 ICHAN = 1,NFREQ
                           RESULT(3,INDEX2) = - ABS (RESULT(3,INDEX2))
                           INDEX2 = INDEX2 + INCFO
 125                       CONTINUE
                        FLGWT = .TRUE.
                        END IF
                     END IF
                  INDEX2 =  (IS - 1) * INCSO + (IIF - 1) * INCIFO + 1
                  DO 130 ICHAN = 1,NFREQ
                     IF (RESULT(3,INDEX2).GT.0.0) SOMEOK = .TRUE.
                     INDEX2 = INDEX2 + INCFO
 130                 CONTINUE
 135              CONTINUE
 140           CONTINUE
            END IF
         IF (SOMEOK) THEN
            IRET = 0
         ELSE
            IRET = 1
            END IF
C                                       last call
      ELSE
         IF (DOFLUX) THEN
            WRITE (MSGTXT,1500) NFLUXF
            IF (NFLUXF.LE.0) MSGTXT = 'No points flagged for excess'
     *         // ' residuals'
            CALL MSGWRT (5)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('Flagged',I9,' time/IF/Stokes samples for excess',
     *   ' residuals')
      END
      SUBROUTINE FIXALB (FIRST, IERR)
C-----------------------------------------------------------------------
C   Reads data from IN2NAME
C   Input:
C      FIRST    L      T => get 2 times worth
C   Output:
C      IERR     I      Error code: > 0 => quit
C   Output in common FIXALBP
C      BPFUNC   R(*)   Bandpass func (complex, chan, pol, if, ant)
C-----------------------------------------------------------------------
      LOGICAL   FIRST
      INTEGER   IERR
C
      CHARACTER PHNAME*48
      INTEGER   I, IIF, IP, IC, J, IA1, IA2, LUNI, INDI, VO, BO, NUMPOL,
     *   NUMIF, NUMFRQ, ILENBU, IPTRI, IBIND, INIO, NCORI, ILWT, NCOPY,
     *   LNCS, LNCF, LNCIF, LT, IVIS, NTAVG
      REAL      SR, SI, SW, BASEN, TIME, TINC, WTFUNC(512,30,30,2,2),
     *   VR, VI, WTSMAX, TAVG, BPR(512), BPI(512)
      COMPLEX   VS
      LOGICAL   EOF, LCOMP
      INCLUDE 'FIXAL.INC'
      INCLUDE 'FIXALBP.INC'
      REAL      WBUFF(UVBFSS)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:PSTD.INC'
      SAVE LCOMP, LNCS, LNCF, LNCIF, INDI, INIO, IBIND, IPTRI, IVIS,
     *   EOF
      DATA VO, BO /0, 1/
      DATA LUNI /31/
C-----------------------------------------------------------------------
      TINC = 15.0 / 60. / 24.0
      IF (DPARM(3).LE.0.0) TINC = 1000.0
      IF (FIRST) THEN
         I = 3600 * 512 * 2
         CALL RFILL (I, 0.0, BPFUNC)
         I = I / 2
         CALL RFILL (I, 0.0, WTFUNC)
         CALL RFILL (3, -100.0, TIMES)
         TAVG = 0.0
         NTAVG = 0
C                                       init 2nd UV data set for read
         CALL COPY (256, CAT2, CATBLK)
         CALL UVPGET (IERR)
         LCOMP = CATBLK(KINAX).EQ.1
         NCORI = (LREC - NRPARM) / CATBLK(KINAX)
         NCOPY = 3 * NCORI
         IF (LCOMP) THEN
            LNCS = INCS * 3
            LNCF = INCF * 3
            LNCIF = INCIF * 3
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *      ILWT, IERR)
         ELSE
            LNCS = INCS
            LNCF = INCF
            LNCIF = INCIF
            END IF
         NUMPOL = CATBLK(KINAX+JLOCS)
         NUMIF = 1
         IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
         NUMFRQ = CATBLK(KINAX+JLOCF)
C                                       open file
         CALL ZPHFIL ('UV', DISKI2, CNO2, 1, PHNAME, IERR)
         CALL ZOPEN (LUNI, INDI, DISKI2, PHNAME, .TRUE., .FALSE.,
     *      .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            GO TO 990
            END IF
         ILENBU = 0
         CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU,
     *      JBUFSZ, BUFF3, BO, IBIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'INIT'
            GO TO 990
            END IF
         LT = 1
         EOF = .FALSE.
         GO TO 90
C                                       copy when possible
      ELSE IF (TIMES(3).GT.0.0) THEN
         I = 3600 * 512
         CALL RCOPY (I, BPFUNC(1,1,1,1,1,2), BPFUNC)
         CALL RFILL (I, 0.0, BPFUNC(1,1,1,1,1,2))
         CALL RFILL (I, 0.0, WTFUNC)
         TIMES(1) = TIMES(2)
         TIMES(2) = TIMES(3)
         LT = 2
         TAVG = 0.0
         NTAVG = 0
C                                       done - do nothing
      ELSE
         IERR = 0
         GO TO 999
         END IF
C                                       record loop
 10   IF (ILOCB.GE.0) THEN
         BASEN = BUFF3(IPTRI+ILOCB)
         IA1 = BASEN / 256 + 0.1
         IA2 = BASEN - IA1 * 256.0 + 0.1
      ELSE
         IA1 = BUFF3(IPTRI+ILOCA1) + 0.1
         IA2 = BUFF3(IPTRI+ILOCA2) + 0.1
         END IF
      IF ((STNNAM(IA1)(:4).EQ.'EVLA') .AND.
     *   (STNNAM(IA2)(:4).EQ.'EVLA')) THEN
         TIME = BUFF3(IPTRI+ILOCT)
         IF (TIMES(LT).LT.-90.) TIMES(LT) = TIME
         IF (TIME-TIMES(LT).LT.TINC) THEN
            TAVG = TAVG + TIME
            NTAVG = NTAVG + 1
            IF (LCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF3(IPTRI+NRPARM),
     *            BUFF3(IPTRI+ILWT), WBUFF)
            ELSE
               CALL RCOPY (NCOPY, BUFF3(IPTRI+NRPARM), WBUFF)
               END IF
            DO 50 IP = 1,NUMPOL
               DO 40 IIF = 1,NUMIF
                  SR = 0.0
                  SI = 0.0
                  SW = 0.0
                  DO 20 IC = CHN1,CHN2
                     J = (IP-1)*LNCS + (IIF-1)*LNCIF + (IC-1)*LNCF +1
                     IF (WBUFF(J+2).GT.0.0) THEN
                        SR = SR + WBUFF(J) * WBUFF(J+2)
                        SI = SI + WBUFF(J+1) * WBUFF(J+2)
                        SW = SW + WBUFF(J+2)
                        END IF
 20                  CONTINUE
                  IF (SW.GT.0.0) THEN
                     SR = SR / SW
                     SI = SI / SW
                     DO 30 IC = 1,NUMFRQ
                        J = (IP-1)*LNCS + (IIF-1)*LNCIF + (IC-1)*LNCF +1
                        IF (WBUFF(J+2).GT.0.0) THEN
                           VS = CMPLX (WBUFF(J)-SR, WBUFF(J+1)-SI) /
     *                        CMPLX (SR, -SI)
                           BPFUNC(IC,IA1,IA2,IP,IIF,LT) = REAL(VS) +
     *                        BPFUNC(IC,IA1,IA2,IP,IIF,LT)
                           BPFUNC(IC,IA2,IA1,IP,IIF,LT) = AIMAG(VS) +
     *                        BPFUNC(IC,IA2,IA1,IP,IIF,LT)
                           WTFUNC(IC,IA1,IA2,IP,IIF) = 1.0 +
     *                        WTFUNC(IC,IA1,IA2,IP,IIF)
                           END IF
 30                     CONTINUE
                     END IF
 40               CONTINUE
 50            CONTINUE
            IPTRI = IPTRI + LREC
            IVIS = IVIS + 1
            IF (IVIS.LE.INIO) GO TO 10
         ELSE
            GO TO 100
            END IF
      ELSE
         IPTRI = IPTRI + LREC
         IVIS = IVIS + 1
         IF (IVIS.LE.INIO) GO TO 10
         END IF
C                                       read
 90   CALL UVDISK ('READ', LUNI, INDI, BUFF3, INIO, IBIND, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ'
         GO TO 990
      ELSE IF ((IERR.EQ.0) .AND. (INIO.GT.0)) THEN
         IPTRI = IBIND
         IVIS = 1
         GO TO 10
         END IF
      TIMES(LT+1) = -100.0
      TIMES(3) = -100.0
      CALL ZCLOSE (LUNI, INDI, IERR)
      EOF = .TRUE.
C                                       average
 100  WTSMAX = 0.0
      DO 150 IA1 = 1,29
         DO 140 IA2 = IA1+1,30
            DO 130 IP = 1,NUMPOL
               DO 120 IIF = 1,NUMIF
                  DO 110 IC = 1,NUMFRQ
                     IF (WTFUNC(IC,IA1,IA2,IP,IIF).LE.0.0) THEN
                        BPFUNC(IC,IA1,IA2,IP,IIF,LT) = FBLANK
                        BPFUNC(IC,IA2,IA1,IP,IIF,LT) = FBLANK
                     ELSE
                        BPFUNC(IC,IA1,IA2,IP,IIF,LT) =
     *                     BPFUNC(IC,IA1,IA2,IP,IIF,LT) /
     *                     WTFUNC(IC,IA1,IA2,IP,IIF)
                        BPFUNC(IC,IA2,IA1,IP,IIF,LT) =
     *                     BPFUNC(IC,IA2,IA1,IP,IIF,LT) /
     *                     WTFUNC(IC,IA1,IA2,IP,IIF)
                        WTSMAX = MAX (WTFUNC(IC,IA1,IA2,IP,IIF),
     *                     WTSMAX)
                        END IF
 110                 CONTINUE
 120              CONTINUE
 130           CONTINUE
 140        CONTINUE
 150     CONTINUE
C                                       averaging: polarization
      IF ((DPARM(1).LE.0.0) .AND. (NUMPOL.GT.1)) THEN
         DO 250 IC = 1,NUMFRQ
            DO 240 IA1 = 1,29
               DO 230 IA2 = IA1+1,30
                  DO 220 IIF = 1,NUMIF
                     SR = 0.0
                     SI = 0.0
                     SW = 0.0
                     DO 200 IP = 1,NUMPOL
                        IF (WTFUNC(IC,IA1,IA2,IP,IIF).GT.0.0) THEN
                           VR = BPFUNC(IC,IA1,IA2,IP,IIF,LT)
                           VI = BPFUNC(IC,IA2,IA1,IP,IIF,LT)
                           SR = SR + VR * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SI = SI + VI * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SW = SW + WTFUNC(IC,IA1,IA2,IP,IIF)
                           END IF
 200                    CONTINUE
                     DO 210 IP = 1,NUMPOL
                        IF (WTFUNC(IC,IA1,IA2,IP,IIF).GT.0.0) THEN
                           BPFUNC(IC,IA1,IA2,IP,IIF,LT) = SR / SW
                           BPFUNC(IC,IA2,IA1,IP,IIF,LT) = SI / SW
                           WTFUNC(IC,IA1,IA2,IP,IIF) = SW
                           END IF
 210                    CONTINUE
 220                 CONTINUE
 230              CONTINUE
 240           CONTINUE
 250        CONTINUE
         END IF
      IF ((DPARM(2).LE.0.0) .AND. (NUMIF.GT.1)) THEN
         DO 350 IC = 1,NUMFRQ
            DO 340 IA1 = 1,29
               DO 330 IA2 = IA1+1,30
                  DO 320 IP = 1,NUMPOL
                     SR = 0.0
                     SI = 0.0
                     SW = 0.0
                     DO 300 IIF = 1,NUMIF
                        IF (WTFUNC(IC,IA1,IA2,IP,IIF).GT.0.0) THEN
                           VR = BPFUNC(IC,IA1,IA2,IP,IIF,LT)
                           VI = BPFUNC(IC,IA2,IA1,IP,IIF,LT)
                           SR = SR + VR * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SI = SI + VI * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SW = SW + WTFUNC(IC,IA1,IA2,IP,IIF)
                           END IF
 300                    CONTINUE
                     DO 310 IIF = 1,NUMIF
                        IF (WTFUNC(IC,IA1,IA2,IP,IIF).GT.0.0) THEN
                           BPFUNC(IC,IA1,IA2,IP,IIF,LT) = SR / SW
                           BPFUNC(IC,IA2,IA1,IP,IIF,LT) = SI / SW
                           WTFUNC(IC,IA1,IA2,IP,IIF) = SW
                           END IF
 310                    CONTINUE
 320                 CONTINUE
 330              CONTINUE
 340           CONTINUE
 350        CONTINUE
         END IF
      IF (DPARM(4).LE.0.0) THEN
         DO 450 IC = 1,NUMFRQ
            DO 440 IP = 1,NUMPOL
               DO 430 IIF = 1,NUMIF
                  SR = 0.0
                  SI = 0.0
                  SW = 0.0
                  DO 410 IA1 = 1,29
                     DO 405 IA2 = IA1+1,30
                        IF (WTFUNC(IC,IA1,IA2,IP,IIF).GT.0.0) THEN
                           VR = BPFUNC(IC,IA1,IA2,IP,IIF,LT)
                           VI = BPFUNC(IC,IA2,IA1,IP,IIF,LT)
                           SR = SR + VR * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SI = SI + VI * WTFUNC(IC,IA1,IA2,IP,IIF)
                           SW = SW + WTFUNC(IC,IA1,IA2,IP,IIF)
                           END IF
 405                    CONTINUE
 410                 CONTINUE
                  SR = SR / SW
                  SI = SI / SW
                  DO 420 IA1 = 1,29
                     DO 415 IA2 = IA1+1,30
                        BPFUNC(IC,IA1,IA2,IP,IIF,LT) = SR
                        BPFUNC(IC,IA2,IA1,IP,IIF,LT) = SI
                        WTFUNC(IC,IA1,IA2,IP,IIF) = SW
 415                    CONTINUE
 420                 CONTINUE
 430              CONTINUE
 440           CONTINUE
 450        CONTINUE
         END IF
C                                       zero imaginary function
      IF (DPARM(5).LE.0.0) THEN
         DO 550 IC = 1,NUMFRQ
            DO 540 IA1 = 1,29
               DO 530 IA2 = IA1+1,30
                  DO 520 IIF = 1,NUMIF
                     SR = 0.0
                     SI = 0.0
                     SW = 0.0
                     DO 510 IP = 1,NUMPOL
                        BPFUNC(IC,IA2,IA1,IP,IIF,LT) = 0.0
 510                    CONTINUE
 520                 CONTINUE
 530              CONTINUE
 540           CONTINUE
 550        CONTINUE
         END IF
C                                       Hanning
      IF (DPARM(6).GT.0.0) THEN
         DO 650 IA1 = 1,29
            DO 640 IA2 = IA1+1,30
               DO 630 IP = 1,NUMPOL
                  DO 620 IIF = 1,NUMIF
                     CALL RCOPY (512, BPFUNC(1,IA1,IA2,IP,IIF,LT), BPR)
                     CALL RCOPY (512, BPFUNC(1,IA2,IA1,IP,IIF,LT), BPI)
                     DO 610 IC = 1,NUMFRQ
                        SR = 0.0
                        SI = 0.0
                        SW = 0.0
                        IF ((IC.GT.1) .AND. (BPR(IC-1).NE.FBLANK)) THEN
                           SR = SR + 0.25 * BPR(IC-1)
                           SI = SI + 0.25 * BPI(IC-1)
                           SW = SW + 0.25
                           END IF
                        IF (BPR(IC).NE.FBLANK) THEN
                           SR = SR + 0.5 * BPR(IC)
                           SI = SI + 0.5 * BPI(IC)
                           SW = SW + 0.5
                           END IF
                        IF ((IC.LT.NUMFRQ) .AND. (BPR(IC+1).NE.FBLANK))
     *                     THEN
                           SR = SR + 0.25 * BPR(IC+1)
                           SI = SI + 0.25 * BPI(IC+1)
                           SW = SW + 0.25
                           END IF
                        IF (SW.LE.0.0) THEN
                           BPFUNC(IC,IA1,IA2,IP,IIF,LT) = FBLANK
                           BPFUNC(IC,IA2,IA1,IP,IIF,LT) = FBLANK
                        ELSE
                           BPFUNC(IC,IA1,IA2,IP,IIF,LT) = SR / SW
                           BPFUNC(IC,IA2,IA1,IP,IIF,LT) = SI / SW
                           END IF
 610                    CONTINUE
 620                 CONTINUE
 630              CONTINUE
 640           CONTINUE
 650        CONTINUE
         END IF
      TIMES(LT) = TAVG / NTAVG
      LT = LT + 1
      TAVG = 0.0
      NTAVG = 0
      I = 3600 * 512
      CALL RFILL (I, 0.0, WTFUNC)
      IF ((LT.EQ.2) .AND. (.NOT.EOF)) GO TO 10
      IF ((LT.EQ.3) .AND. (.NOT.EOF)) TIMES(3) = TIME
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXALB: ERROR',I5,' DOING ',A,' OF UV FILE')
      END

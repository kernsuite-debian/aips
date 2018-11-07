LOCAL INCLUDE 'SPMOD.INC'
C                                       Local include for SPMOD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MAXGAU
      PARAMETER (MAXGAU = 9999)
C
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   NGAUSS, CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO
      LOGICAL   ISCOMP, DOSPIX
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XXSTOK, XINLST(12)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, INLIST*48
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XBIF, XEIF, XBCHAN, XECHAN, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH(3), XDOAC, XSOUT,
     *   XDISO, FLUX, FACTOR, QUAL, DOHIST, BADD(10)
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS), DXCG(MAXGAU),
     *   DYCG(MAXGAU), DZCG(MAXGAU), ZEROSP(MAXGAU,2), GPOS(2,MAXGAU),
     *   GWID(3,MAXGAU), SCENTR(MAXGAU), SWIDTH(MAXGAU)
      INTEGER   ITYPE(MAXGAU)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XXSTOK, XTIME, XBAND, XFREQ, XFQID, XSUBA, XBIF, XEIF, XBCHAN,
     *   XECHAN, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH, XDOAC, XNAMOU, XCLAOU, XSOUT, XDISO, XINLST,
     *   FLUX, FACTOR, QUAL, DOHIST, BADD
      COMMON /UVMPRM/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, ISCOMP,
     *   ILOCWT, NGAUSS, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO, DOSPIX
      COMMON /UVMDLS/ GPOS, GWID, DXCG, DYCG, DZCG, ZEROSP, SCENTR,
     *   SWIDTH, ITYPE
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, INLIST
LOCAL END
      PROGRAM SPMOD
C-----------------------------------------------------------------------
C! Add source model to uv data
C# UV Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 2013, 2015, 2018
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
C   SPMOD inserts model visibilities into a data base adding noise.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      FLUX           FLUX          Noise level in Jy.
C      FACTOR         FACTOR        Multiplication factor for previous
C                                   data to be added to current.
C      QUAL           QUAL          If greater than zero then reset
C                                   weights to a value of 1.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'SPMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'SPMOD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SPMDIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SPMDUV (IRET)
C                                       History, copy tables
      IF (IRET.EQ.0) CALL SPMDHI
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE SPMDIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   SPMDIN gets input parameters for SPMOD and creates an output file
C   if necessary.
C-----------------------------------------------------------------------
      CHARACTER STAT*4, PRGN*6, UTYPE*2
      INTEGER   JERR, IROUND, NPARM, IERR, LUN, I, NFREQ, INCX
      LOGICAL   T, MATCH
      REAL      RPARM(20)
      HOLLERITH CATH(256)
      INCLUDE 'SPMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATBLK, CATH)
      DATA T /.TRUE./
      DATA LUN /20/
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
      NPARM = 191
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      DO 5 I = 1,10
         IBAD(I) = IROUND(BADD(I))
 5       CONTINUE
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (4, 1, XXSTOK, STOKES)
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (48, 1, XINLST, INLIST)
C                                       Info for UVGET:
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      DOACOR = XDOAC.GT.0.0
C                                       Set time range.
      CALL RCOPY (8, XTIME, TIMRNG)
      IF ((TIMRNG(1)+TIMRNG(2)+TIMRNG(3)+TIMRNG(4)) .EQ.0.0)
     *   TIMRNG(1)=-1.0E6
      IF ((TIMRNG(5)+TIMRNG(6)+TIMRNG(7)+TIMRNG(8)) .EQ.0.0)
     *   TIMRNG(5)=1.0E6
      TSTART = TIMRNG(1) + TIMRNG(2) / 24. + TIMRNG(3) / (24. * 60.) +
     *   TIMRNG(4) / (24. * 60. * 60.)
      TEND = TIMRNG(5) + TIMRNG(6) / 24. + TIMRNG(7) / (24. * 60.) +
     *   TIMRNG(8) / (24. * 60. * 60.)
      DOPOL = IROUND(XDOPOL)
      IF (XDOPOL.GT.0.0) DOPOL = MAX (1, DOPOL)
      PDVER = IROUND (XPDVER)
      DOAPPL = .FALSE.
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LT.0) SUBARR = 0
      FGVER = IROUND (XFLAG)
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      CLVER = IROUND (XGUSE)
      CLUSE = IROUND (XGUSE)
      BLVER = IROUND (XBLVER)
C                                       Source parms
      IF (INLIST.EQ.' ') THEN
         MSGTXT = 'INPUT MODEL TEXT FILE MUST BE GIVEN'
         IERR = 10
         GO TO 990
      ELSE
         CALL READIT (IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
      DO 25 I = 1,NGAUSS
         ITYPE(I) = ITYPE(I) + 1
         IF ((ITYPE(I).LT.1) .OR. (ITYPE(I).GT.6)) ITYPE(I) = 1
         IF (ITYPE(I).EQ.5) THEN
            GWID(2,I) = GWID(1,I)
            GWID(3,I) = 0.0
            END IF
 25      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      IF (ICOR0.GT.0) THEN
         IF (NCOR.EQ.1) THEN
            STOKES = 'I'
         ELSE
            IF (STOKES.NE.'HALF') STOKES = 'IV'
            END IF
      ELSE
         IF ((STOKES.NE.'I') .AND. (STOKES.NE.'IV')) STOKES = 'HALF'
         END IF
C                                       If compressed, uncompress
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Channel selection?
      IF (JLOCIF.LT.0) THEN
         BIF = 1
         EIF = 1
      ELSE
         BIF = IROUND (XBIF)
         EIF = IROUND (XEIF)
         BIF = MIN (MAX (1, BIF), CATBLK(KINAX+JLOCIF))
         IF (EIF.LT.BIF) EIF = CATBLK(KINAX+JLOCIF)
         END IF
      NFREQ = CATBLK(KINAX+JLOCF)
      BCHAN = IROUND (XBCHAN)
      ECHAN = IROUND (XECHAN)
      IF ((BCHAN.LE.0) .OR. (BCHAN.GT.NFREQ)) BCHAN = 1
      IF ((ECHAN.LE.0) .OR. (ECHAN.GT.NFREQ)) ECHAN = NFREQ
      IF (BCHAN.GT.ECHAN) THEN
         MSGTXT = 'INVALID BCHAN AND ECHAN'
         CALL MSGWRT (6)
         JERR = 1
         GO TO 990
         END IF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      LUN = 28
      CALL FQMATC (DISKIN, OLDCNO, CATBLK, LUN, SELBAN, SELFRQ, MATCH,
     *   FRQSEL, JERR)
      IF (.NOT.MATCH) THEN
         MSGTXT = 'NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS'
         JERR = 1
         GO TO 990
         END IF
      IF (JERR.GT.0) GO TO 999
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      CALL UVGET ('CLOS', RPARM, BUFF1, IERR)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       check that single source by now
      CALL AXEFND (6, 'SOURCE', CATBLK(KIPCN), CATH(KHPTP), I, IERR)
      IF (IERR.EQ.0) THEN
         MSGTXT = 'MORE THAN ONE SOURCE SELECTED: NOT ALLOWED'
         JERR = 10
         GO TO 990
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
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
      CALL UVCREA (DISKO, CCNO, BUFF1, JERR)
      IF (JERR.NE.0) THEN
         IF (JERR.NE.2) THEN
            WRITE (MSGTXT,1050) JERR
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1065) JERR
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
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, JERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPMDIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('SPMDIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SPMDUV (IRET)
C-----------------------------------------------------------------------
C   Routine to read data, send it to SPMODL to add a model and/or
C   noise and write the output file.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48
      INTEGER   IRET, IPTRO, LUNO, INDO, NCORO, ILENBU, KBIND, NIOUT,
     *   NIOLIM, I, IA1, IA2, BO, VO, NCOPY, NUMVIS, XCOUNT, NNCOR,
     *   CATMP(256), RNXRET
      DOUBLE PRECISION XDEC, XRA
      LOGICAL   T, F
      INCLUDE 'SPMOD.INC'
      REAL      DUM, BASEN, VIS(UVBFSS), RPARM(20), ROTATE, CATR(256)
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      MSGSUP = 32000
      CALL ROTFND (CATR, ROTATE, IRET)
      MSGSUP = 0
      DO 10 I = 1,NGAUSS
         CALL XYSHFT (RA, DEC, GPOS(1,I), GPOS(2,I), ROTATE, XRA, XDEC)
         IF (TYPUVD.EQ.0) THEN
            CALL SHISIN (RA, DEC, ROTATE, XRA, XDEC, DXCG(I), DYCG(I),
     *         DZCG(I))
         ELSE IF (TYPUVD.EQ.-1) THEN
            CALL SHINCP (RA, DEC, ROTATE, XRA, XDEC, DXCG(I), DYCG(I),
     *         DZCG(I))
         ELSE
            DXCG(I) = TWOPI * GPOS(1,I) * DG2RAD / 3600.0D0
            DYCG(I) = TWOPI * GPOS(2,I) * DG2RAD / 3600.0D0
            DZCG(I) = 0.0
            END IF
         GWID(1,I) = GWID(1,I) * DG2RAD / 3600.0D0
         GWID(2,I) = GWID(2,I) * DG2RAD / 3600.0D0
 10      CONTINUE

      CALL RANDIN (IRET)
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, NEWCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
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
      NUMVIS = 0
      XCOUNT = 0
      NNCOR = LREC - NRPARM
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
      IF ((FREQ.GT.0.0D0) .AND. (UVFREQ.GT.0.0D0)) THEN
         UVSCAL = FREQ / UVFREQ
      ELSE
         UVSCAL = 1.0D0
         END IF
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
C                                       Call user routine.
         CALL SPMODL (NUMVIS, RPARM(1+ILOCU), RPARM(1+ILOCV),
     *      RPARM(1+ILOCW), VIS, IRET)
C                                       Error (fatal)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1120) IRET
            GO TO 990
C                                       Copy to output.
         ELSE IF (IRET.EQ.0) THEN
            XCOUNT = XCOUNT + 1
            RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
            RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
            RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, VIS, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, VIS, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
         GO TO 100
         END IF
C                                       Final call to SPMODL.
      NUMVIS = -1
      CALL SPMODL (NUMVIS, DUM, DUM, DUM, VIS, IRET)
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
      CALL UCMPRS (NVIS, DISKO, NEWCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close NX table
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
 1000 FORMAT ('SPMDUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('SPMDUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('SPMDUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1100 FORMAT ('SPMDUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SPMDUV: SPMODL ERROR',I3)
 1150 FORMAT ('SPMDUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE SPMDHI
C-----------------------------------------------------------------------
C   SPMDHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTYP(6)*4
      INTEGER   LUN1, LUN2, IERR, I, J
      LOGICAL   T
      INCLUDE 'SPMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN1, LUN2 /27,28/
      DATA CTYP /'POIN', 'GAUS', 'DISK', 'RECT', 'SPHE', 'EXP '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO,
     *   CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 60
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 60
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 60
C
      IF ((NGAUSS.LE.4) .OR. (DOHIST.GT.0.0)) THEN
         DO 30 I = 1,NGAUSS
            WRITE (HILINE,1400) TSKNAM, I, CTYP(ITYPE(I))
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            MSGTXT = HILINE
            CALL MSGWRT (3)
            IF (IERR.NE.0) GO TO 60
            WRITE (HILINE,1410) TSKNAM, I, ZEROSP(I,1)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            MSGTXT = HILINE
            CALL MSGWRT (3)
            IF (IERR.NE.0) GO TO 60
            WRITE (HILINE,1411) TSKNAM, I, ZEROSP(I,2)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            MSGTXT = HILINE
            CALL MSGWRT (3)
            IF (IERR.NE.0) GO TO 60
            DO 25 J = 1,2
               WRITE (HILINE,1420) TSKNAM, J, I, GPOS(J,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
 25            CONTINUE
            IF (ITYPE(I).NE.1) THEN
               GWID(1,I) = GWID(1,I) * 3600. / DG2RAD
               GWID(2,I) = GWID(2,I) * 3600. / DG2RAD
               WRITE (HILINE,1430) TSKNAM, I, GWID(1,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
               WRITE (HILINE,1431) TSKNAM, I, GWID(2,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
               WRITE (HILINE,1432) TSKNAM, I, GWID(3,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
               END IF
            IF (DOSPIX) THEN
               WRITE (HILINE,1433) TSKNAM, I, SCENTR(I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
               WRITE (HILINE,1434) TSKNAM, I, SWIDTH(I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               MSGTXT = HILINE
               CALL MSGWRT (3)
               IF (IERR.NE.0) GO TO 60
               END IF
 30         CONTINUE
         END IF
C                                       noise
      WRITE (HILINE,1440) TSKNAM, FLUX
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 60
C
      WRITE (HILINE,1445) TSKNAM, FACTOR
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      MSGTXT = HILINE
      CALL MSGWRT (3)
      IF (IERR.NE.0) GO TO 60
C
      IF (QUAL.GT.0.0) THEN
         WRITE (HILINE,1450) TSKNAM
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         MSGTXT = HILINE
         CALL MSGWRT (3)
         END IF
C
 60   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'SPMDHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPMDHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1400 FORMAT (A6,'TYPE(',I4.3,') = ''',A4,'''')
 1410 FORMAT (A6,'RMAX(',I4.3,') = ',1PE12.4,' / in Jy.')
 1411 FORMAT (A6,'LMAX(',I4.3,') = ',1PE12.4,' / in Jy.')
 1420 FORMAT (A6,'GPOS(',I1,',',I4.3,') = ',1PE12.4,' / in Arcsec.')
 1430 FORMAT (A6,'BMAJ(',I4.3,') = ',1PE12.4,' / FWHM in arcsec')
 1431 FORMAT (A6,'BMIN(',I4.3,') = ',1PE12.4,' / FWHM in arcsec')
 1432 FORMAT (A6,'BPA(',I4.3,') = ',1PE12.4,' / in Deg.')
 1433 FORMAT (A6,'CENTER(',I4.3,') = ',1PE12.4,' / line center chan')
 1434 FORMAT (A6,'WIDTH(',I4.3,') = ',1PE12.4,
     *   ' / line FWHM')
 1440 FORMAT (A6,'FLUX = ',1PE12.4,' / noise added in Jy./Wgt.')
 1445 FORMAT (A6,'FACTOR = ',1PE12.4)
 1450 FORMAT (A6,24X,'/ weights reset to 1.0')
      END
      SUBROUTINE SPMODL (NUMVIS, U, V, W, VIS, IRET)
C-----------------------------------------------------------------------
C   Subroutine to add a model or noise to data.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      VIS(3,*) R    Visibilities in order real, imaginary, weight
C                      (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON
C      FREQ       D    Frequency of observation (Hz)
C      CATBLK(256)I    Catalog header record. See Going Aips for
C                      details.
C
C   Output:
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   For RR,LL,RL,LR take the following relations:
C   RR => RR + (I - V)*EXP(iCOMPHA)
C   LL => LL + (I + V)*EXP(iCOMPHA)
C   RL => RL + (Q + iU)*EXP(iCOMPHA)
C   and   LR => LR + (Q - iU)*EXP(iCOMPHA)
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
      REAL      U, V, W, VIS(3,*)
C
      INCLUDE 'SPMOD.INC'
      INTEGER   I, IVIS, INDEX, LIMIT, NIF, NCHAN, NPOLN, IIF, ICHAN,
     *   LUN, BUFFER(512), IIVER, FREQID, J, IC
      REAL      TEMP, COMVIS, COMPHA, CPHI, SPHI, UU, VV, UF, VF, WF,
     *   A, LVIS(2,4), YEROSP(MAXGAU,2), YLUX, CATR(256), RR, AMPVIS,
     *   YSP(MAXGAU)
      DOUBLE PRECISION FRQFAC, R
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   ISBAND(MAXIF)
      DOUBLE PRECISION  FOFF(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      SAVE YLUX, YEROSP, YSP, NCHAN, NPOLN
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (CATR, CATBLK)
      DATA LUN /25/
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LT.0) GO TO 999
C                                       Setup: IF table
      IF (NUMVIS.EQ.1) THEN
         IIVER = 1
         FREQID = FRQSEL
         CALL CHNDAT ('READ', BUFFER, DISKIN, OLDCNO, IIVER, CATOLD,
     *      LUN, NIF, FOFF, ISBAND, FINC, BNDCOD, FREQID, IRET)
         IF (IRET.NE.0) GO TO 999
C                                        No. IFs.
         NIF = 1
         IF (JLOCIF.GT.0) NIF = CATBLK(KINAX+JLOCIF)
C                                       No. Channels
         NCHAN = 1
         IF (JLOCF.GT.0) NCHAN = CATBLK(KINAX+JLOCF)
C                                       No. polarizations.
         NPOLN = 1
         IF (JLOCS.GT.0) NPOLN = CATBLK(KINAX+JLOCS)
         CALL RCOPY (2*MAXGAU, ZEROSP, YEROSP(1,1))
         YLUX = MAX (0.0, FLUX)
         TEMP = 4.0 * LOG (2.0)
         CALL RFILL (NGAUSS, TEMP, YSP)
         DO 10 I = 1,NGAUSS
            IF (SWIDTH(I).GT.0.0) YSP(I) = YSP(I) / (SWIDTH(I)**2)
 10         CONTINUE
         END IF
C                                       Loop over IF
      A = 3.14159 / LOG (2.0)
      A = A * A
      IC = 0
      DO 600 IIF = BIF,EIF
C                                       Loop over Channels
         DO 550 ICHAN = 1,NCHAN
            IC = IC + 1
C                                       Scale U,V,W to frequency.
            FRQFAC = (UVFREQ + FOFF(IIF) + (ICHAN - CATR(KRCRP+JLOCF)) *
     *         FINC(IIF)) / UVFREQ
            UF = U * FRQFAC
            VF = V * FRQFAC
            WF = W * FRQFAC
C                                       Visibility index
            INDEX = (ICHAN-1) * INCFI + (IIF-1) * INCIFI + 1
C                                       Calculate vis
            CALL RFILL (8, 0.0, LVIS)
            DO 60 J = 1,NGAUSS
               AMPVIS = YSP(J) * ((IC - SCENTR(J)) ** 2)
               IF (AMPVIS.LE.10.0) THEN
                  AMPVIS = EXP (-AMPVIS)
                  COMPHA = UF*DXCG(J) + VF*DYCG(J) + WF*DZCG(J)
                  CPHI = 6.283185 / 360.0
                  SPHI = SIN (GWID(3,J) * CPHI)
                  CPHI = COS (GWID(3,J) * CPHI)
                  UU = GWID(1,J) * (VF*CPHI + UF*SPHI)
                  VV = GWID(2,J) * (UF*CPHI - VF*SPHI)
                  R = SQRT (UU**2+VV**2)
                  IF (ITYPE(J).EQ.2) THEN
                     COMVIS = EXP (-3.559707 * R**2)
                  ELSE IF (ITYPE(J).EQ.3) THEN
                     RR = R
                     CALL BESSEL (RR, COMVIS)
                  ELSE IF (ITYPE(J).EQ.4) THEN
                     CALL SINC (UU, TEMP)
                     CALL SINC (VV, COMVIS)
                     COMVIS = COMVIS * TEMP
                  ELSE IF (ITYPE(J).EQ.5) THEN
                     RR = R
                     CALL SPHEUV (RR, COMVIS)
                  ELSE IF (ITYPE(J).EQ.6) THEN
                     COMVIS = 1.0 / (1.0 + A*R*R) ** 1.5
                  ELSE
                     COMVIS = 1.0
                     END IF
                  COMVIS = COMVIS * AMPVIS
C                                       RR,LL,RL,LR
                  IF (ICOR0.LE.0) THEN
C                                       Do the RR and LL pairs.
                     LIMIT = 2
                     LIMIT = MIN (NPOLN, LIMIT)
                     DO 30 I = 1,LIMIT
                        LVIS(1,I) = LVIS(1,I) + COMVIS * COS (COMPHA) *
     *                     YEROSP(J,I)
                        LVIS(2,I) = LVIS(2,I) + COMVIS * SIN (COMPHA) *
     *                     YEROSP(J,I)
 30                     CONTINUE
C                                       True I,Q,U,V
                  ELSE
                     DO 50 I = 1,NPOLN
                        LVIS(1,I) = 0.5 * COMVIS * COS (COMPHA) *
     *                     (YEROSP(J,1) + YEROSP(J,2)) + LVIS(1,I)
                        LVIS(2,I) = 0.5 * COMVIS * *SIN (COMPHA) *
     *                     (YEROSP(J,1) - YEROSP(J,2)) + LVIS(2,I)
 50                     CONTINUE
                     END IF
                  END IF
 60            CONTINUE
            DO 70 I = 1,NPOLN
               IVIS = INDEX + (I-1) * INCSI
               IF (QUAL.GT.0.0) VIS(3,IVIS) = 1.0
               IF (VIS(3,IVIS).GT.0.0) THEN
                  VIS(1,IVIS) = VIS(1,IVIS)*FACTOR + LVIS(1,I)
                  VIS(2,IVIS) = VIS(2,IVIS)*FACTOR + LVIS(2,I)
C                                       Add noise to all pairs
                  IF (FLUX.GT.0.0) THEN
                     COMVIS = YLUX / SQRT (VIS(3,IVIS))
                     CALL NOISE (TEMP)
                     VIS(1,IVIS) = VIS(1,IVIS) + TEMP * COMVIS
                     CALL NOISE (TEMP)
                     VIS(2,IVIS) = VIS(2,IVIS) + TEMP * COMVIS
                     END IF
                  END IF
 70            CONTINUE
C                                       End Channel loop
 550        CONTINUE
C                                       End IF loop
 600     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE SINC (X, ASINC)
C-----------------------------------------------------------------------
C   Compute ASINC = Sin (pi * X) / (Pi * X)
C    Input:  X       R    Argument
C    Output: ASINC   R    Result
C-----------------------------------------------------------------------
      REAL   X, ASINC
C-----------------------------------------------------------------------
      ASINC = 1.0
      IF (ABS(X).LE.1.0E-20) GO TO 999
         X = 3.14159 * X
         ASINC = SIN(X) / X
C
 999  RETURN
      END
      SUBROUTINE BESSEL (X, ABES)
C-----------------------------------------------------------------------
C   Compute Bessel function (J1)
C   Input:
C      X       R    Argument
C   Output:
C      ABES    R    Result
C-----------------------------------------------------------------------
      REAL      X, ABES, BESSJ1
      REAL      Y
C-----------------------------------------------------------------------
      IF (ABS(X).LE.1.0E-10) THEN
         ABES = 1.0
      ELSE
         Y = 3.141592565 * X
         ABES = 2.0 * BESSJ1 (Y) / Y
         END IF
C
 999  RETURN
      END
      SUBROUTINE SPHEUV (X, ABES)
C-----------------------------------------------------------------------
C   Compute spheroid FT
C   Input:
C      X       R    Argument
C   Output:
C      ABES    R    Result
C-----------------------------------------------------------------------
      REAL      X, ABES
      REAL      Y
C-----------------------------------------------------------------------
      Y = 3.14159 * MAX (X, 0.02)
      ABES = 3. * ((SIN(Y) / Y) - COS(Y)) / (Y * Y)
C
 999  RETURN
      END
      SUBROUTINE NOISE (ANOISE)
C-----------------------------------------------------------------------
C   Random noise generator
C    Output: ANOISE  R    Result
C-----------------------------------------------------------------------
      REAL      ANOISE, TEMP
      INTEGER   J
C-----------------------------------------------------------------------
      ANOISE = -6.0
      DO 10 J = 1,12
         CALL RANDUM (TEMP)
         ANOISE = ANOISE + TEMP
 10      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE BESJ (X, N, BJ, D, IER)
C-----------------------------------------------------------------------
C   Compute the J Bessel function for a given argument and order
C   REPLACED BY BESSJ1 FUNCTION FOLLOWING
C   Inputs:
C      X     R    The argument of the J Bessel function desired
C      N     I    The order of the J Bessel function desired
C      D     R    Required accuracy
C      IER   I    Resultant error code where
C                   IER=0  No error
C                   IER=1  N is negative
C                   IER=2  X is negative or zero
C                   IER=3  Required accuracy not obtained
C                   IER=4  Range of N compared to X not correct
C                          (See Remarks)
C     Output:
C      BJ    R    The resultant J Bessel function
C
C   Remarks
C      N must be greater than or equal to zero, but it must be <
C         20+10*X-X** 2/3   FOR X LESS THAN OR EQUAL TO 15
C         90+X/2           FOR X GREATER THAN 15
C   Method
C      Recurrence relation technique described by H. Goldstein and
C      R.M. Thaler, 'Recurrence Techniques for the Calculation of
C      Bessel Functions', M.T.A.C., v.13, pp.102-108 and I.A. Stegun
C      and M. Abramowitz, 'Generation of Bessel Functions on High
C      Speed Computers', M.T.A.C., v.11, 1957, pp.255-257.
C-----------------------------------------------------------------------
      INTEGER   IER, N, NTEST, IN1, MA, MB, MZERO, MMAX, M, JT, M2, K,
     *   MK
      REAL   BJ, D, X, BPREV, FM1, FM, ALPHA, BMK, S
C-----------------------------------------------------------------------
C                                       Test inputs
      BJ = 0.0
      IF (N.LT.0) THEN
         IER = 1
         GO TO 999
         END IF
      IF (X.LE.0) THEN
         IER = 2
         GO TO 999
         END IF
      IF (X.LE.15.) NTEST = 20. + 10.*X - X ** 2/3
      IF (X.GT.15.) NTEST = 90. + X / 2.
      IF (N.GE.NTEST) THEN
         IER = 4
         GO TO 999
         END IF
      IER = 0
      IN1 = N + 1
      BPREV = .0
C                                       Compute starting value of M
      MA = X + 6.
      IF (X.GE.5.) MA = 1.4 * X + 60. / X
      MB = N + INT(X) / 4 + 2
      MZERO = MAX (MA, MB)
C                                       Set upper limit of M
      MMAX = NTEST
      DO 190 M = MZERO,MMAX,3
C                                       Set F(M), F(M-1)
         FM1 = 1.0E-28
         FM = 0.0
         ALPHA = 0.0
         JT = 1
         IF (M.EQ.(M/2)*2) JT = -1
         M2 = M - 2
         DO 160 K = 1,M2
            MK = M - K
            BMK = 2.0 * REAL(MK) * FM1/X - FM
            FM = FM1
            FM1 = BMK
            IF (MK.EQ.N+1) BJ = BMK
            JT = -JT
            S = 1 + JT
            ALPHA = ALPHA + BMK*S
 160        CONTINUE
         BMK = 2.0 * FM1/X - FM
         IF (N.EQ.0) BJ = BMK
         ALPHA = ALPHA + BMK
         BJ = BJ / ALPHA
         IF (ABS(BJ-BPREV).LE.ABS(D*BJ)) GO TO 999
            BPREV = BJ
 190     CONTINUE
      IER = 3
C
 999  RETURN
      END
      FUNCTION BESSJ1 (X)
C-----------------------------------------------------------------------
C   Compute the J1 Bessel function for a given argument
C   Inputs:
C      X     R    The argument of the J1 Bessel function desired
C   Output:
C            R    Returns the resultant J1 Bessel function
C   Method
C      from "Numerical Recipes" by Press et al.
C-----------------------------------------------------------------------
      REAL BESSJ1, X
C
      REAL AX, XX, Z
      DOUBLE PRECISION P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2,
     *   R3, R4, R5, R6, S1, S2, S3, S4, S5, S6, Y
      SAVE P1, P2, P3, P4, P5, Q1, Q2, Q3, Q4, Q5, R1, R2, R3, R4, R5,
     *   R6, S1, S2, S3, S4, S5, S6
      DATA R1, R2, R3, R4, R5, R6 /72362614232.D0, -7895059235.D0,
     *   242396853.1D0, -2972611.439D0, 15704.48260D0, -30.16036606D0/
      DATA S1, S2, S3, S4, S5, S6 /144725228442.D0, 2300535178.D0,
     *   18583304.74D0, 99447.43394D0, 376.9991397D0, 1.D0/
      DATA P1, P2, P3, P4, P5 /1.D0, .183105D-2, -.3516396496D-4,
     *   .2457520174D-5, -.240337019D-6/
      DATA Q1, Q2, Q3, Q4, Q5 /.04687499995D0, -.2002690873D-3,
     *   .8449199096D-5, -.88228987D-6, .105787412D-6/
C-----------------------------------------------------------------------
      IF (ABS(X).LT.8.) THEN
         Y = X**2
         BESSJ1 = X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /
     *      (S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
         AX = ABS (X)
         Z = 8. / AX
         Y = Z**2
         XX = AX - 2.356194491
         BESSJ1 = SIGN (1., X) * SQRT (.636619772/AX) *
     *      (COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y*P5)))) -
     *       Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
         END IF
C
 999  RETURN
      END
      SUBROUTINE READIT (IERR)
C-----------------------------------------------------------------------
C   Reads INLIST to make a list of coordinates of sources
C   Outputs:
C      IERR   I   Error code: > 0 quit
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'SPMOD.INC'
      INTEGER   TLUN, INGAUS, TIND, JTRIM, LLIM, LP
      CHARACTER LINE*132
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA TLUN /3/
C-----------------------------------------------------------------------
C                                       open the text file
      CALL ZTXOPN ('READ', TLUN, TIND, INLIST, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN TEXT FILE'
         GO TO 980
         END IF
C                                       defaults
      CALL RFILL (2*MAXGAU, 0.0, ZEROSP)
      CALL RFILL (2*MAXGAU, 0.0, GPOS)
      CALL RFILL (3*MAXGAU, 0.0, GWID)
      CALL RFILL (MAXGAU, 0.0, SCENTR)
      CALL RFILL (MAXGAU, 0.0, SWIDTH)
      CALL FILL (MAXGAU, 0, ITYPE)
      INGAUS = NGAUSS
      NGAUSS = 0
C                                       read until end or max
 10   CALL ZTXIO ('READ', TLUN, TIND, LINE, IERR)
      IF ((IERR.EQ.0) .AND. (NGAUSS.LT.MAXGAU)) THEN
         LLIM = JTRIM (LINE)
         IF (LLIM.LE.0) GO TO 10
         IF ((LINE(:1).EQ.'#') .OR. (LINE(:1).EQ.';')) GO TO 10
C                                       flux
         LP = 1
         CALL GETNUM (LINE, LLIM, LP, X)
         IF (X.EQ.DBLANK) GO TO 900
         NGAUSS = NGAUSS + 1
         ZEROSP(NGAUSS,1) = X
         ZEROSP(NGAUSS,2) = X
C                                       position
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            GPOS(1,NGAUSS) = X
            END IF
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            GPOS(2,NGAUSS) = X
            END IF
C                                       width
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            GWID(1,NGAUSS) = X
            END IF
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            GWID(2,NGAUSS) = X
            END IF
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            GWID(3,NGAUSS) = X
            END IF
C                                       type
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            ITYPE(NGAUSS) = X + 0.1
            END IF
C                                       line center, width
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            SCENTR(NGAUSS) = X
            END IF
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            SWIDTH(NGAUSS) = X
            END IF
C                                       LL flux
         IF (LP.LE.LLIM) THEN
            CALL GETNUM (LINE, LLIM, LP, X)
            IF (X.EQ.DBLANK) GO TO 900
            ZEROSP(NGAUSS,2) = X
            END IF
C                                       check that there is something
         IF ((ZEROSP(NGAUSS,1).EQ.0.0) .AND. (ZEROSP(NGAUSS,2).EQ.0.0))
     *      NGAUSS = NGAUSS - 1
         GO TO 10
C                                       real error
      ELSE IF ((IERR.GT.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1000) IERR, 'READING TEXT FILE'
         GO TO 980
C                                       EOF
      ELSE
         CALL ZTXCLS (TLUN, TIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'CLOSING TEXT FILE'
            GO TO 980
            END IF
         END IF
      WRITE (MSGTXT,1010) NGAUSS
      CALL MSGWRT (2)
      IF (NGAUSS.LE.0) IERR = 8
      GO TO 999
C                                       format error
 900  WRITE (MSGTXT,1900) NGAUSS
      IERR = 10
C
 980  CALL MSGWRT (8)
      IF (TIND.GT.0) CALL ZTXCLS (TLUN, TIND, LP)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('READIT ERROR',I3,2X,A)
 1010 FORMAT ('READIT: read',I5,' model components')
 1900 FORMAT ('READIT FORMAT ERROR NEAR LINE',I5)
      END

LOCAL INCLUDE 'REWAY.INC'
C                                       Local include for REWAY
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XCALC, XNAMOU(3),
     *   XCLAOU(2), XOUTXT(12), XOPTYP
      REAL      XSIN, XDISIN, XQUAL, XTIME(8), XBAND, XFREQ, XFQID,
     *   XSUBA, XDOCAL, XGUSE, XDOPOL, XPDVER, XBLVER, XFLAG, XDOBND,
     *   XBPVER, XSMOTH(3), XDOAC, XSOUT, XDISO, XCHNS(4,20), XOUFG,
     *   APARM(10), BPARM(10), XCENT, XBADD(10),
     *   SCRBUF(256), BUFF1(UVBFSS), BUFF2(UVBFSS), BUFF3(UVBFSS),
     *   RMSMIN, RMSMAX, BUFFS(MAXCHA,2), DIFPIX,
     *   MXWTS(MAXANT,MAXANT,MAXIF,2), MNWTS(MAXANT,MAXANT,MAXIF,2),
     *   SUMWTS(MAXANT,MAXANT,MAXIF,2), SUMSQW(MAXANT,MAXANT,MAXIF,2)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     *   LRECO, NRPRMI, NRPRMO, OLDCNO, NEWCNO, CHNSEL(3,20,MAXIF),
     *   NIF, NSTOK, NANT, NCHAN, COUNT(6), FGVERO, FGVERI, NOWAY,
     *   NSUMS(MAXANT,MAXANT,MAXIF,2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(10)*64,
     *   OUTEXT*48, OPTYPE*4
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XSOUR, XQUAL, XCALC,
     *   XTIME, XBAND, XFREQ, XFQID, XSUBA, XDOCAL, XGUSE, XDOPOL,
     *   XPDVER, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XDOAC, XNAMOU,
     *   XCLAOU, XSOUT, XDISO, XCHNS, XOUFG, APARM, BPARM, XOUTXT,
     *   XOPTYP, XCENT, XBADD
      COMMON /REWAYP/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECO,
     *   NRPRMI, NRPRMO, OLDCNO, NEWCNO, CHNSEL, NIF, NSTOK, NANT,
     *   NCHAN, RMSMIN, RMSMAX, COUNT, FGVERO, FGVERI, NOWAY, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD, OUTEXT,
     *   OPTYPE
      COMMON /BUFRS/ SCRBUF, BUFF1, BUFF2, BUFF3, JBUFSZ, BUFFS
      COMMON /STATS/ SUMWTS, SUMSQW, MXWTS, MNWTS, NSUMS
C                                       End local include for REWAY
LOCAL END
      PROGRAM REWAY
C-----------------------------------------------------------------------
C! Allows user to provide subroutine to operate on UV data base
C# Utility UV UV-util VLA VLB Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2010-2012, 2014-2017
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
C   REWAY does SPLIT/SPLAT while calculating weights based on the rms
C   in the spectram of each IF and polarization.  The usual calibration
C   adverbs STOKES, BCHAN, and ECHAN are suppressed.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C   full set of calibration adverbs
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      ICHANSEL
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET, NWORDS, IERR, NBL, NTIME
      REAL      S(2), AV(2), B(2), VM(2)
      LONGINT   PS, PAV, PB, PVM
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'REWAY '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RWAYIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       BL-based weights
      IF (APARM(1).EQ.0.0) THEN
C                                       simple ones
         IF (APARM(3).LE.0.0) THEN
            CALL RWAYUV (IRET)
C                                       smooth/clip
         ELSE
            IF (APARM(3).LE.0.0) APARM(3) = 1.0
            NWORDS = 3 * NANT * NANT * NIF * NSTOK
            NWORDS = MAX (NWORDS, 5120)
            NWORDS = (NWORDS - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', PRGM, NWORDS, S, PS, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (8)
               GO TO 990
               END IF
            CALL RWAYBL (NANT, NIF, S(1+PS), IRET)
            CALL ZMEMRY ('FREE', PRGM, NWORDS, S, PS, IERR)
            END IF
C                                       time average, ant-based weights
      ELSE IF (APARM(1).LT.0.0) THEN
         IF (APARM(3).LE.0.0) APARM(3) = 1.0
         NWORDS = NANT * NANT * NIF * NSTOK
         NWORDS = MAX (NWORDS, 5120)
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWORDS, AV, PAV, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NWORDS = 3 * NANT * NIF * NSTOK
         NWORDS = MAX (NWORDS, 5120)
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWORDS, S, PS, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         CALL RWAYAN (NANT, NIF, S(1+PS), AV(1+PAV), IRET)
         CALL ZMEMRY ('FREE', PRGM, NWORDS, S, PS, IERR)
C                                       time average, bl-based weights
      ELSE
         IF (APARM(3).LE.0.0) APARM(3) = 1.0
         NWORDS = NANT * NANT * NIF * 4
         NWORDS = MAX (NWORDS, 5120)
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWORDS, AV, PAV, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NWORDS = 3 * NANT * NANT * NIF * NSTOK
         NWORDS = MAX (NWORDS, 5120)
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWORDS, S, PS, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         NBL = (NANT * (NANT+1)) / 2
         NTIME = APARM(1) + 0.1
         NTIME = (NTIME/2)*2 + 1
         NTIME = MIN (MAX (3, NTIME), 99)
         APARM(1) = NTIME
         NWORDS = 3 * NCHAN * NTIME * NBL * NIF * NSTOK
         NWORDS = (NWORDS - 1) / 1024 + 1
         CALL ZMEMRY ('GET ', PRGM, NWORDS, B, PB, IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
            CALL MSGWRT (8)
            GO TO 990
            END IF
         IF (OPTYPE(:3).EQ.'MED') THEN
            NWORDS = 2 * NTIME * NCHAN
            NWORDS = (NWORDS - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', PRGM, NWORDS, VM, PVM, IRET)
            IF (IRET.NE.0) THEN
               MSGTXT = 'UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (8)
               GO TO 990
               END IF
            END IF
         CALL RWAYFB (NCHAN, NTIME, NANT, NBL, NIF, B(1+PB), S(1+PS),
     *      AV(1+PAV), VM(1+PVM), IRET)
         CALL ZMEMRY ('FREE', PRGM, NWORDS, AV, PAV, IERR)
         CALL ZMEMRY ('FREE', PRGM, NWORDS, B, PB, IERR)
         IF (OPTYPE(:3).EQ.'MED') CALL ZMEMRY ('FREE', PRGM, NWORDS, VM,
     *      PVM, IERR)
         CALL ZMEMRY ('FREE', PRGM, NWORDS, S, PS, IERR)
         END IF
      IF (IRET.EQ.0) THEN
         CALL RWAYHI
         IF (BPARM(2).GT.0.0) THEN
            IF (APARM(1).LT.0.0) THEN
               CALL REPANT (IRET)
            ELSE
               CALL REPBL (IRET)
               END IF
            END IF
         END IF
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRBUF)
C
 999  STOP
      END
      SUBROUTINE RWAYIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   RWAYIN gets input parameters for REWAY and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      NRPRMI  I  Input number of random parameters.
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
      INCLUDE 'REWAY.INC'
      CHARACTER STAT*4, BLANK*6, PTYPE*2, KEYWRD*8
      INTEGER   IROUND, NPARM, IERR, INCX, I, LUN, NW(MAXIF), K, K1, K2,
     *   J, NUMKEY, LOCS, VALUE, KEYTYP
      LOGICAL   MATCH
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
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 285
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
      CALL H2CHR (4, 1, XCALC, SELCOD)
      CALL H2CHR (48, 1, XOUTXT, OUTEXT)
      CALL H2CHR (4, 1, XOPTYP, OPTYPE)
      IF ((OPTYPE.NE.'MEDI') .AND. (OPTYPE.NE.'MEDR')) OPTYPE = 'RMSR'
      STOKES = ' '
      DO 10 I = 1,30
         CALL H2CHR (16, 1, XSOUR(1,I), SOURCS(I))
 10      CONTINUE
C                                       BADDISK
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 20      CONTINUE
      SELQUA = IROUND (XQUAL)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
      NOWAY = IROUND (APARM(9))
      IF (APARM(9).GT.0.0) NOWAY = MAX (1, NOWAY)
      I = MAXANT * MAXANT * MAXIF * 2
      CALL FILL (I, 0, NSUMS)
      CALL RFILL (I, 0.0, MXWTS)
      CALL RFILL (I, 0.0, MNWTS)
      CALL RFILL (I, 0.0, SUMWTS)
      CALL RFILL (I, 0.0, SUMSQW)
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
C                                       flag table versions
      CALL FNDEXT ('FG', CATBLK, I)
      IF ((FGVER.EQ.0) .OR. (FGVER.GT.I)) FGVER = I
      IF (I.EQ.0) FGVER = -1
      FGVERO = IROUND (XOUFG)
      IF (FGVERO.LE.0) FGVERO = -1
      IF (FGVERO.GT.I) FGVERO = I + 1
      IF ((FGVERO.EQ.FGVER) .AND. (FGVER.GT.0)) THEN
         MSGTXT = 'FLAGVER and OUTFGVER cannot be the same, making new'
         CALL MSGWRT (6)
         FGVERO = I + 1
         END IF
      FGVERI = FGVER
      IF (FGVERO.LE.I) FGVERI = -ABS (FGVERI)
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
      NCHAN = CATBLK(KINAX+JLOCF)
      BCHAN = 1
      ECHAN = NCHAN
      IF ((NOWAY.GT.0) .AND. (ILOCIT.LT.0)) NOWAY = 2
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
C                                       Channel selection
      I = 60 * MAXIF
      CALL FILL (I, 0, CHNSEL)
      CALL FILL (MAXIF, 0, NW)
      DO 40 J = 1,20
         K = IROUND (XCHNS(2,J))
         IF (K.GT.0) THEN
            K = IROUND (XCHNS(4,J))
            IF ((K.LE.0) .OR. (K.GT.MAXIF)) THEN
               K1 = 1
               K2 = MAXIF
            ELSE
               K1 = K
               K2 = K
               END IF
            DO 35 K = K1,K2
               NW(K) = NW(K) + 1
               DO 30 I = 1,3
                  CHNSEL(I,NW(K),K) = IROUND (XCHNS(I,J))
                  IF (CHNSEL(I,NW(K),K).LT.0) CHNSEL(I,NW(K),K) = 0
 30               CONTINUE
               IF (CHNSEL(3,NW(K),K).EQ.0) CHNSEL(3,NW(K),K) = 1
 35            CONTINUE
            END IF
 40      CONTINUE
C                                       If no channel selection
C                                       use VLA definition of
C                                       channel 0
      BCHAN = NCHAN
      ECHAN = 1
      DO 50 K = 1,MAXIF
         IF (NW(K).LE.0) THEN
            NW(K) = 1
            CHNSEL(1,1,K) = (NCHAN+1)/8 + 1
            CHNSEL(2,1,K) = NCHAN - ((NCHAN+1)/8)
            CHNSEL(3,1,K) = 1
            END IF
         DO 45 I = 1,NW(K)
            CHNSEL(1,I,K) = MAX (1, MIN (CHNSEL(1,I,K), NCHAN))
            IF (CHNSEL(2,I,K).LT.CHNSEL(1,I,K))
     *         CHNSEL(2,I,K) = NCHAN
            CHNSEL(2,I,K) = MAX (1, MIN (CHNSEL(2,I,K), NCHAN))
            BCHAN = MIN (BCHAN, CHNSEL(1,I,K))
            ECHAN = MAX (ECHAN, CHNSEL(2,I,K))
 45         CONTINUE
 50      CONTINUE
C                                       is this valid thing to do
      IF (ECHAN-BCHAN.LT.3) THEN
         JERR = 10
         MSGTXT = 'TOO FEW SPECTRAL CHANNELS TO COMPUTE RMS'
         GO TO 990
      ELSE IF (ECHAN-BCHAN.LT.10) THEN
         MSGTXT = 'NUMBER SPECTRAL CHANNELS PRETTY LOW FOR THIS OP'
         CALL MSGWRT (7)
         END IF
      CALL FNDEXT ('BP', CATBLK, K1)
      CALL FNDEXT ('CL', CATBLK, K2)
      IF (K2.EQ.0) CALL FNDEXT ('SN', CATBLK, K2)
      IF ((DOBAND.LE.0) .AND. (K1.GT.0)) THEN
         MSGTXT = 'WARNING: BANDPASS TABLE IS NOT BEING APPLIED'
         CALL MSGWRT (7)
         END IF
      IF ((.NOT.DOCAL) .AND. (K2.GT.0)) THEN
         MSGTXT = 'WARNING: CL/SN TABLES ARE NOT BEING APPLIED'
         CALL MSGWRT (7)
         END IF
      BCHAN = 1
      ECHAN = NCHAN
C                                       now using cal system -
C                                       UVGET makes header
      CALL UVGET ('INIT', RPARM, SCRBUF, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1035) JERR
         GO TO 990
         END IF
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      NSTOK = CATBLK(KINAX+JLOCS)
      CALL UVGET ('CLOS', RPARM, SCRBUF, IERR)
C                                       get max antenna number
      CALL GETANT (DISKIN, OLDCNO, SUBARR, CATUV, SCRBUF, IERR)
      NANT = NSTNS
C                                       Save input file info
      INCX = CATBLK(KINAX)
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       center frequency?
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
      ELSE
         DIFPIX = 0.0
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
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
C                                       CROSSPOW keyword
      KEYWRD = 'CROSSPOW'
      NUMKEY = 1
      CALL CATKEY ('REED', DISKIN, OLDCNO, KEYWRD, NUMKEY, LOCS,
     *   VALUE, KEYTYP, SCRBUF, IERR)
      IF ((IERR.NE.0) .OR. (ABS(VALUE).GT.1) .OR. (KEYTYP.EQ.0)) THEN
         MSGTXT = 'DON''T KNOW IF INPUT FILE IS CROSS POWER OR NOT'
         CALL MSGWRT (6)
         LOCS = 1
         VALUE = 1
         KEYTYP = 4
         END IF
C                                       Copy any header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
C                                       set CROSSPOW to 0 or -1
      VALUE = MIN (0, VALUE)
      CALL CATKEY ('WRIT', DISKO, NEWCNO, KEYWRD, NUMKEY, LOCS,
     *   VALUE, KEYTYP, SCRBUF, IERR)
      IF (APARM(5).LE.1.0) APARM(5) = 4.0
      IF (APARM(6).LE.1.0) APARM(6) = 10000.
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1035 FORMAT ('UVGET INIT ERROR',I3,' CHECK ADVERBS')
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('RWAYIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE RWAYUV (IRET)
C-----------------------------------------------------------------------
C   RWAYUV sends uv data one point at a time to the rms finding routine
C   and then writes the modified data if requested.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'REWAY.INC'
      CHARACTER OFILE*48
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET,
     *   CHFLGS(MAXCIF)
      LOGICAL   T, F, GOTONE
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20), XF
      DOUBLE PRECISION UVSCAL
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
      EQUIVALENCE (RESULT, BUFF3)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       counters, mask
      NIF = 1
      IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
      NCHAN = CATBLK(KINAX+JLOCF)
      NSTOK = CATBLK(KINAX+JLOCS)
      CALL CHWANT (NCHAN, NIF, CHNSEL, CHFLGS)
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      COUNT(6) = 0
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT VIS DATA'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT VIS DATA'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INIT OUTPUT VIS DATA'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
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
         WRITE (MSGTXT,1000) IRET, 'READ INPUT VIS DATA'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,100000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
C                                       call user routine
         CALL RWAYIT (NSTOK, NCHAN, NIF, CHFLGS, VIS, RPARM, RESULT,
     *      COUNT, GOTONE)
         IF (GOTONE) THEN
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE OUTPUT VIS DATA'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSH OUTPUT VIS DATA'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
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
      XF = 100.0 * COUNT(2)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1310) XF
      CALL MSGWRT (4)
      XF = 100.0 * COUNT(3)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1311) XF
      CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYUV: ERROR',I3,' ON ',A)
 1100 FORMAT ('RWAYUV: on visibility',I10)
 1310 FORMAT (F6.3,' % of spectra had Real/Imag rms > 1.5')
 1311 FORMAT (F6.3,' % of spectra had Imag/Real rms > 1.5')
      END
      SUBROUTINE CHWANT (NCH, NIF, CHNSEL, CHFLGS)
C-----------------------------------------------------------------------
C   Makes a mask of the desired channels
C   Inputs:
C      NCH      I            Number spectral chans
C      NIF      I            Number IFs
C      CHNSEL   I(3,20,*)    Start, stop, incr 20 sets per IF
C   Outputs
C      CHFLGS   I(*,*)       1.0 => use, 0.0 => don't use
C-----------------------------------------------------------------------
      INTEGER   NCH, NIF, CHNSEL(3,20,*), CHFLGS(NCH,NIF)
C
      INTEGER   I, J, K
C-----------------------------------------------------------------------
      J = NCH * NIF
      CALL RFILL (J, 0.0, CHFLGS)
      DO 30 K = 1,NIF
         DO 20 J = 1,20
            IF ((CHNSEL(1,J,K).GT.0) .AND. (CHNSEL(3,J,K).GT.0) .AND.
     *         (CHNSEL(2,J,K).GE.CHNSEL(1,J,K))) THEN
               DO 10 I = CHNSEL(1,J,K),CHNSEL(2,J,K),CHNSEL(3,J,K)
                  CHFLGS(I,K) = 1
 10               CONTINUE
               END IF
 20         CONTINUE
 30      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RWAYHI
C-----------------------------------------------------------------------
C   RWAYHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, TYPE(3)*12
      INTEGER   LUN1, LUN2, IERR, I, J
      REAL      XF
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA TYPE /'Gaussian','Exponential','Boxcar'/
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
C                                       calibration history
      CALL CALHIS (LUN2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      DO 20 I = BIF,EIF
         DO 10 J = 1,20
            IF (CHNSEL(1,J,I).GT.0) THEN
               WRITE (HILINE,1010) TSKNAM, J, I, CHNSEL(1,J,I),
     *            CHNSEL(2,J,I), CHNSEL(3,J,I)
               CALL HIADD (LUN2, HILINE, BUFF2, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 10         CONTINUE
 20      CONTINUE
      IF (APARM(1).LT.0.0) THEN
         WRITE (HILINE,1020) TSKNAM, -APARM(1)
      ELSE IF (APARM(1).EQ.0.0) THEN
         HILINE = TSKNAM // '/ baseline-dependent weights' //
     *      ' no time average'
      ELSE
         I = APARM(1) + 0.1
         WRITE (HILINE,1021) TSKNAM, I
         END IF
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      IF (APARM(3).GT.0.0) THEN
         WRITE (HILINE,1022) TSKNAM, APARM(3)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         I = APARM(4) + 1.1
         I = MAX (1, MIN (3, I))
         WRITE (HILINE,1023) TSKNAM, TYPE(I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1024) TSKNAM, APARM(5)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         IF ((APARM(6).GT.0.0) .AND. (APARM(6).LT.1000.)) THEN
            WRITE (HILINE,1025) TSKNAM, APARM(6)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         WRITE (HILINE,1026) TSKNAM, APARM(7)
         IF (APARM(7).GT.0.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1027) TSKNAM, APARM(8)
         IF (APARM(8).LT.1.E6) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1028) TSKNAM, RMSMIN
         IF (RMSMIN.GT.1.01E-6) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1029) TSKNAM, RMSMAX
         IF (RMSMIN.LT.1.0E9) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         END IF
      IF (COUNT(1).GT.0) THEN
         XF = (100.0 * COUNT(2)) / COUNT(1)
         WRITE (HILINE,1030) TSKNAM, XF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         XF = (100.0 * COUNT(3)) / COUNT(1)
         WRITE (HILINE,1031) TSKNAM, XF
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         XF = (100.0 * COUNT(4)) / COUNT(1)
         WRITE (HILINE,1032) TSKNAM, XF
         IF (COUNT(4).GT.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         WRITE (HILINE,1033) TSKNAM, COUNT(5)
         IF (COUNT(5).GT.0) CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
         IF (COUNT(6).GT.0) THEN
            WRITE (HILINE,1034) TSKNAM, COUNT(6)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            I = BPARM(1) + 0.1
            WRITE (HILINE,1035) TSKNAM, I
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 100
            END IF
         END IF
      WRITE (HILINE,1040) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1045) TSKNAM, APARM(9)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Close HI file
 100  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL COPTAB (DISKIN, OLDCNO, DISKO, NEWCNO, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'RWAYHI: ERROR COPYING TABLES TO OUTPUT UV'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, NEWCNO, DIFPIX, BUFF2(1025), BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'CENTHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                       will want to delete WT table
C                                       Update CATBLK.
      IF (APARM(1).GT.0.0) THEN
         IF (APARM(10).LE.0.0) THEN
            MSGTXT = 'Deleting temporary WT table'
            CALL MSGWRT (2)
            CALL RMEXT (DISKO, NEWCNO, 'WT', 0, CATBLK, SCRBUF, IERR)
         ELSE
            MSGTXT = 'Keeping WT table'
            CALL MSGWRT (2)
            CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF,
     *         IERR)
            END IF
      ELSE
         CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRBUF,
     *      IERR)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'CHANSEL(,',I2,',',I2,') =',I5,',',I5,',',I2)
 1020 FORMAT (A6,'APARM(1)=',F7.1,' / time average secs.',
     *   ' weights antenna based')
 1021 FORMAT (A6,'APARM(1)=',I3,' / rolling buffer # times,',
     *   'weights baseline based')
 1022 FORMAT (A6,'APARM(3)=',F7.1,' / FWHM time smooth antenna weights',
     *   ' seconds')
 1023 FORMAT (A6,'SMOTYPE=''',A,''' / smoothing function type')
 1024 FORMAT (A6,'SMOCUT=',F5.2,' / omit points > SMOCUT from average')
 1025 FORMAT (A6,'TOTCUT=',F5.2,
     *   ' / omit points > TOTCUT from overall average')
 1026 FORMAT (A6,'FLAGLOW =',F8.1,' / flag points < FLAGLOW')
 1027 FORMAT (A6,'FLAGHIGH=',F8.1,' / flag points > FLAGHIGH')
 1028 FORMAT (A6,'RMSMIN=',F9.5,' / flag points < RMSMIN')
 1029 FORMAT (A6,'RMSMAX=',F9.5,' / flag points > RMSMAX')
 1030 FORMAT (A6,'/ ',F7.4,' % samples had Real/Imag rms > 1.5')
 1031 FORMAT (A6,'/ ',F7.4,' % samples had Imag/Real rms > 1.5')
 1032 FORMAT (A6,'/ ',F7.4,' % samples flagged due to clipping')
 1033 FORMAT (A6,'/ ',I10,' spectra previously flagged')
 1034 FORMAT (A6,'/ ',I10,' spectra too few samples')
 1035 FORMAT (A6,'BPARM(1)=',I5,' / minimum number samples')
 1040 FORMAT (A6,'OPTYPE =''',A,'''  / method to find rms')
 1045 FORMAT (A6,'APARM(9)=',F4.0,'  / > 0 => ignore input weights')
      END
      SUBROUTINE RWAYIT (NS, NF, NI, CHFLGS, VIS, RPARM, RESULT, CNT,
     *   GOTONE)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS   I      Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed
C      NS       I      # Stokes in data
C      NF       I      # spectral channels
C      NI       I      # IFs
C      CHFLGS   I      (NF,NIF) selection flags
C      RPARM    R(*)   Random parameter array which includes U,V,W etc
C                      but also any other random parameters.
C      VIS      R(3,*)   Visibilities in order real, imaginary, weight
C                      (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   In/out:
C      CNT      I(2)   Count (1) valid # spectra (2) ones that Re did
C                      not match Im
C   Output:
C      RESULT   R(3,*)   Output visibilities selected in frequency.
C      GOTONE   L      T => there is some valid data in RESULT
C-----------------------------------------------------------------------
      INTEGER   NS, NF, NI, CHFLGS(NF,NI), CNT(6)
      REAL      VIS(3,*), RPARM(*), RESULT(3,*)
      LOGICAL   GOTONE
C
      INTEGER   JIF, JF, JS, INDEXO, INDEXI, INDI, INDO, IT, L, LR, LI,
     *   LT, JA1, JA2, KA1, KA2, KP
      REAL      RMSR, RMSI, WS(10), MEDIAN, WAIN, BASEN, RT
      DOUBLE PRECISION SR, SSR, SI, SSI, WR, WI, V, WT, AVR, AVI
C                                       DEBUG
      DOUBLE PRECISION W1, EPS
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA WS /6.0, 5.0, 4.5, 4.3, 4.0, 3.8, 3.5, 3.2, 3.0, 3.0/
C-----------------------------------------------------------------------
C                                       antenna numbers
      IF (ILOCB.GE.0) THEN
         BASEN = RPARM(1+ILOCB)
         JA1 = BASEN / 256. + 0.1
         JA2 = BASEN - JA1*256. + 0.1
      ELSE
         JA1 = RPARM(1+ILOCA1) + 0.1
         JA2 = RPARM(1+ILOCA2) + 0.1
         END IF
C                                       robust rms/mean
      IF (OPTYPE.EQ.'RMSR') THEN
         DO 100 JIF = 1,NI
            DO 90 JS = 1,NS
               KP = (JS + 1) / 2
               IF (MOD(JS,2).EQ.0) THEN
                  KA1 = JA2
                  KA2 = JA1
               ELSE
                  KA1 = JA1
                  KA2 = JA2
                  END IF
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               INDO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
               AVR = 0.0D0
               AVI = 0.0D0
               RMSR = 1000.0
               RMSI = 1000.0
               DO 30 IT = 1,10
                  SR = 0.0D0
                  SSR = 0.0D0
                  WR = 0.0D0
                  LR = 0
                  SI = 0.0D0
                  SSI = 0.0D0
                  WI = 0.0D0
                  LI = 0
                  LT = 0
C                                       DEBUG
                  W1 = 0.0D0
                  EPS = 0.0D0
                  INDEXI = INDI
                  INDEXO = INDO
                  DO 20 JF = 1,NF
                     WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                     IF (WT.GT.0.0) THEN
                        LT = LT + 1
                        IF (NOWAY.GE.2) THEN
                           WT = 1.0D0
                        ELSE IF (NOWAY.EQ.1) THEN
                           WT = MAX (1.0, RPARM(1+ILOCIT))
                           END IF
C                                       DEBUG
                        IF (W1.LE.0.0D0) THEN
                           W1 = WT
                           EPS = 0.001D0 * WT
                        ELSE
                           IF (ABS(W1-WT).GT.EPS) THEN
                              MSGTXT = 'OOPS'
                              CALL MSGWRT (1)
                              END IF
                           END IF
                        V = VIS(1,INDEXI)
                        IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                           SR = SR + V * WT
                           SSR = SSR + V * V * WT
                           WR = WR + WT
                           LR = LR + 1
                           END IF
                        V = VIS(2,INDEXI)
                        IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                           SI = SI + V * WT
                           SSI = SSI + V * V * WT
                           WI = WI + WT
                           LI = LI + 1
                           END IF
                        END IF
                     INDEXI = INDEXI + INCFI
 20                  CONTINUE
                  IF (WR.GT.0.0) THEN
                     AVR = SR / WR
                     SSR = SSR / WR
                     RMSR = SSR - AVR * AVR
                     RMSR = SQRT (MAX (0.0, RMSR))
                     END IF
                  IF (WI.GT.0.0) THEN
                     AVI = SI / WI
                     SSI = SSI / WI
                     RMSI = SSI - AVI * AVI
                     RMSI = SQRT (MAX (0.0, RMSI))
                     END IF
 30               CONTINUE
C                                       check result
               WAIN = 1.0
               IF ((WR.LE.0.0) .AND. (WI.LE.0.0)) THEN
                  WT = 0.0D0
                  CNT(5) = CNT(5) + 1
               ELSE IF (LT.LT.BPARM(1)) THEN
                  WT = 0.0D0
                  CNT(6) = CNT(6) + 1
               ELSE
                  CNT(1) = CNT(1) + 1
C                                       NOWAY not needed in this line
                  WAIN = (WR + WI) / (LR + LI)
                  IF ((WR.GT.0.0D0) .AND. (WI.LE.0.0D0)) THEN
                     CNT(3) = CNT(3) + 1
                     RMSI = RMSR
                  ELSE IF ((WR.LE.0.0D0) .AND. (WI.GT.0.0D0)) THEN
                     CNT(2) = CNT(2) + 1
                     RMSR = RMSI
                     END IF
                  RMSI = MAX (1.E-6, RMSI)
                  RMSR = MAX (1.E-6, RMSR)
                  WT = RMSR * RMSI
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                  WT = 1.0 / WT
                  IF (NSUMS(KA1,KA2,JIF,KP).LE.0) THEN
                     MNWTS(KA1,KA2,JIF,KP) = WT
                     MXWTS(KA1,KA2,JIF,KP) = WT
                     SUMWTS(KA1,KA2,JIF,KP) = WT
                     SUMSQW(KA1,KA2,JIF,KP) = WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = 1
                  ELSE
                     RT = WT
                     MNWTS(KA1,KA2,JIF,KP) =
     *                  MIN (MNWTS(KA1,KA2,JIF,KP),RT)
                     MXWTS(KA1,KA2,JIF,KP) =
     *                  MAX (MXWTS(KA1,KA2,JIF,KP),RT)
                     SUMWTS(KA1,KA2,JIF,KP) = SUMWTS(KA1,KA2,JIF,KP) +
     *                  WT
                     SUMSQW(KA1,KA2,JIF,KP) = SUMSQW(KA1,KA2,JIF,KP) +
     *                  WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = NSUMS(KA1,KA2,JIF,KP) + 1
                     END IF
                  END IF
C                                       apply result
               INDEXI = INDI
               INDEXO = INDO
               GOTONE = .FALSE.
               DO 80 JF = 1,NF
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        RESULT(3,INDEXO) = WT
                     ELSE IF (NOWAY.EQ.1) THEN
                        RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT) / WAIN
                     ELSE
                        RESULT(3,INDEXO) = WT * VIS(3,INDEXI) / WAIN
                        END IF
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
                  IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 80               CONTINUE
 90            CONTINUE
 100        CONTINUE
C                                       median method
      ELSE IF (OPTYPE.EQ.'MEDI') THEN
         DO 200 JIF = 1,NI
            DO 190 JS = 1,NS
               KP = (JS + 1) / 2
               IF (MOD(JS,2).EQ.0) THEN
                  KA1 = JA2
                  KA2 = JA1
               ELSE
                  KA1 = JA1
                  KA2 = JA2
                  END IF
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               INDO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
               L = 0
               INDEXI = INDI
               WAIN = 0.0
               DO 110 JF = 1,NF
                  WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                  IF (WT.GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        WT = 1.0D0
                     ELSE IF (NOWAY.EQ.1) THEN
                        WT = MAX (1.0, RPARM(1+ILOCIT))
                        END IF
                     WAIN = WAIN + WT
                     L = L + 1
                     BUFFS(L,1) = VIS(1,INDEXI)
                     BUFFS(L,2) = VIS(2,INDEXI)
                     END IF
                  INDEXI = INDEXI + INCFI
 110              CONTINUE
               IF ((L.GT.0) .AND. (L.LT.BPARM(1))) THEN
                  CNT(6) = CNT(6) + 1
                  WT = 0.0D0
               ELSE IF (L.GT.0) THEN
                  WAIN = WAIN / L
                  AVR = MEDIAN (L, BUFFS(1,1))
                  AVI = MEDIAN (L, BUFFS(1,2))
                  DO 120 IT = 1,L
                     BUFFS(IT,1) = ABS (BUFFS(IT,1) - AVR)
                     BUFFS(IT,2) = ABS (BUFFS(IT,2) - AVI)
 120                 CONTINUE
                  RMSR = 1.4826 * MEDIAN (L, BUFFS(1,1))
                  RMSI = 1.4826 * MEDIAN (L, BUFFS(1,2))
                  CNT(1) = CNT(1) + 1
                  RMSI = MAX (1.E-6, RMSI)
                  RMSR = MAX (1.E-6, RMSR)
                  WT = RMSR * RMSI
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                  WT = 1.0 / WT
                  IF (NSUMS(KA1,KA2,JIF,KP).LE.0) THEN
                     MNWTS(KA1,KA2,JIF,KP) = WT
                     MXWTS(KA1,KA2,JIF,KP) = WT
                     SUMWTS(KA1,KA2,JIF,KP) = WT
                     SUMSQW(KA1,KA2,JIF,KP) = WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = 1
                  ELSE
                     RT = WT
                     MNWTS(KA1,KA2,JIF,KP) =
     *                  MIN (MNWTS(KA1,KA2,JIF,KP),RT)
                     MXWTS(KA1,KA2,JIF,KP) =
     *                  MAX (MXWTS(KA1,KA2,JIF,KP),RT)
                     SUMWTS(KA1,KA2,JIF,KP) = SUMWTS(KA1,KA2,JIF,KP) +
     *                  WT
                     SUMSQW(KA1,KA2,JIF,KP) = SUMSQW(KA1,KA2,JIF,KP) +
     *                  WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = NSUMS(KA1,KA2,JIF,KP) + 1
                     END IF
C                                       check result
               ELSE
                  WT = 0.0D0
                  CNT(5) = CNT(5) + 1
                  END IF
C                                       apply result
               INDEXI = INDI
               INDEXO = INDO
               GOTONE = .FALSE.
               DO 180 JF = 1,NF
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        RESULT(3,INDEXO) = WT
                     ELSE IF (NOWAY.EQ.1) THEN
                        RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT) / WAIN
                     ELSE
                        RESULT(3,INDEXO) = WT * VIS(3,INDEXI) / WAIN
                        END IF
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
                  IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 180              CONTINUE
 190           CONTINUE
 200        CONTINUE
C                                       robust median
      ELSE
         DO 300 JIF = 1,NI
            DO 290 JS = 1,NS
               KP = (JS + 1) / 2
               IF (MOD(JS,2).EQ.0) THEN
                  KA1 = JA2
                  KA2 = JA1
               ELSE
                  KA1 = JA1
                  KA2 = JA2
                  END IF
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               INDO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
               AVR = 0.0D0
               AVI = 0.0D0
               RMSR = 1000.0
               RMSI = 1000.0
               DO 240 IT = 1,10,2
                  INDEXI = INDI
                  INDEXO = INDO
                  LR = 0
                  LI = 0
                  WR = 0.0D0
                  WI = 0.0D0
                  LT = 0
                  DO 220 JF = 1,NF
                     WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                     IF (WT.GT.0.0) THEN
                        LT = LT + 1
                        IF (NOWAY.GE.2) THEN
                           WT = 1.0D0
                        ELSE IF (NOWAY.EQ.1) THEN
                           WT = MAX (1.0, RPARM(1+ILOCIT))
                           END IF
                        V = VIS(1,INDEXI)
                        IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                           LR = LR + 1
                           BUFFS(LR,1) = V
                           WR = WR + WT
                           END IF
                        V = VIS(2,INDEXI)
                        IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                           LI = LI + 1
                           BUFFS(LI,2) = V
                           WI = WI + WT
                           END IF
                        END IF
                     INDEXI = INDEXI + INCFI
 220                 CONTINUE
                  IF (LR.GT.0.0) THEN
                     AVR = MEDIAN (LR, BUFFS(1,1))
                     DO 225 L = 1,LR
                        BUFFS(L,1) = ABS (BUFFS(L,1)-AVR)
 225                    CONTINUE
                     RMSR = 1.4826 * MEDIAN (LR, BUFFS(1,1))
                     END IF
                  IF (LI.GT.0.0) THEN
                     AVI = MEDIAN (LI, BUFFS(1,2))
                     DO 230 L = 1,LI
                        BUFFS(L,2) = ABS (BUFFS(L,2)-AVI)
 230                    CONTINUE
                     RMSI = MEDIAN (LI, BUFFS(1,2))
                     END IF
 240              CONTINUE
C                                       check result
               WAIN = 1.0
               IF ((LR.LE.0) .AND. (LI.LE.0)) THEN
                  WT = 0.0D0
                  CNT(5) = CNT(5) + 1
               ELSE IF (LT.LT.BPARM(1)) THEN
                  WT = 0.0D0
                  CNT(6) = CNT(6) + 1
               ELSE
C                                       NOWAY not neede here
                  WAIN = (WR + WI) / (LR + LI)
                  CNT(1) = CNT(1) + 1
                  IF ((LR.GT.0) .AND. (LI.LE.0)) THEN
                     CNT(3) = CNT(3) + 1
                     RMSI = RMSR
                  ELSE IF ((LR.LE.0) .AND. (LI.GT.0)) THEN
                     CNT(2) = CNT(2) + 1
                     RMSR = RMSI
                     END IF
                  RMSI = MAX (1.E-6, RMSI)
                  RMSR = MAX (1.E-6, RMSR)
                  WT = RMSR * RMSI
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                  WT = 1.0 / WT
                  IF (NSUMS(KA1,KA2,JIF,KP).LE.0) THEN
                     MNWTS(KA1,KA2,JIF,KP) = WT
                     MXWTS(KA1,KA2,JIF,KP) = WT
                     SUMWTS(KA1,KA2,JIF,KP) = WT
                     SUMSQW(KA1,KA2,JIF,KP) = WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = 1
                  ELSE
                     RT = WT
                     MNWTS(KA1,KA2,JIF,KP) =
     *                  MIN (MNWTS(KA1,KA2,JIF,KP),RT)
                     MXWTS(KA1,KA2,JIF,KP) =
     *                  MAX (MXWTS(KA1,KA2,JIF,KP),RT)
                     SUMWTS(KA1,KA2,JIF,KP) = SUMWTS(KA1,KA2,JIF,KP) +
     *                  WT
                     SUMSQW(KA1,KA2,JIF,KP) = SUMSQW(KA1,KA2,JIF,KP) +
     *                  WT * WT
                     NSUMS(KA1,KA2,JIF,KP) = NSUMS(KA1,KA2,JIF,KP) + 1
                     END IF
                  END IF
C                                       apply result
               INDEXI = INDI
               INDEXO = INDO
               GOTONE = .FALSE.
               DO 280 JF = 1,NF
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        RESULT(3,INDEXO) = WT
                     ELSE IF (NOWAY.EQ.1) THEN
                        RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT) / WAIN
                     ELSE
                        RESULT(3,INDEXO) = WT * VIS(3,INDEXI) / WAIN
                        END IF
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
                  IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 280              CONTINUE
 290           CONTINUE
 300        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWAYAN (NA, NI, S, AV, IRET)
C-----------------------------------------------------------------------
C   RWAYAN sends uv data one point at a time to the rms finding routine
C   The weights are averaged and converted to antenna based.  They are
C   then stored in a table.  This table may be sorted, smoothed, and
C   re-sorted.  Then the data and table are re-read and the antenna
C   based weights applied to the data.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Inputs:
C      NA      I      Max antenna number
C      NI      I      Max IF in data
C   Output:
C      S       R(*)   work array
C      AV      R(*)   work array
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NA, NI, IRET
      REAL      S(3,NA,NI,*), AV(NA,NA,NI,*)
C
      INCLUDE 'REWAY.INC'
      CHARACTER OFILE*48, OPCODE*4, TITLE(5)*24, UNITS(5)*8
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET, FVIS,
     *   CHFLGS(MAXCIF), LUN, NKEY, NREC, NCOL, DATP(128,2), VER,
     *   TABUFF(512), RECORD(4+4*MAXIF), JA1, JA2, JI, JP, NT,
     *   ISOU, LSOU, NW, NTIMES, NW1, NW2, I, JA
      LOGICAL   T, F, DOAVG, END, GOTONE
      REAL      VIS(UVBFSS), RESULT(UVBFSS), RPARM(20), RECWTS(4*MAXIF),
     *   TB, TE, SOLINT, SASQ(MAXANT), TS, FRACT(MAXANT), BASEN, XF,
     *   WRKBUF(2), WTSBUF(2), RMSS(4,MAXIF,MAXANT), RECR(4+4*MAXIF),
     *   SA(2), SSA, W
      LONGINT   PWRK, PWTS
      DOUBLE PRECISION UVSCAL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
      EQUIVALENCE (RESULT, BUFF3)
      EQUIVALENCE (RECORD, RECR)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA TITLE /'START VIS','END VIS','ANTENNA','TIME','ANT WEIGHTS'/
      DATA UNITS /' ', ' ', ' ', 'DAYS', ' '/
C-----------------------------------------------------------------------
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, NIF, CHNSEL, CHFLGS)
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      COUNT(6) = 0
      NTIMES = 0
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       make special table
      MSGTXT = 'Build the WT table'
      CALL MSGWRT (2)
      NKEY = 0
      NREC = 1000
      NCOL = 5
      CALL FILL (256, 0, DATP)
      DATP(1,2) = 14
      DATP(2,2) = 14
      DATP(3,2) = 14
      DATP(4,2) = 12
      DATP(5,2) = 10 * (4 * NIF) + 2
      VER = 1
      LUN = 77
      CALL TABINI ('WRIT', 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING TEMP TABLE'
         GO TO 990
         END IF
      DO 10 I = 1,NCOL
         CALL CHR2H (24, TITLE(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 3, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
         CALL CHR2H (8, UNITS(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
 10      CONTINUE
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      TB = -1000.0
      TE = TB
      NW = 3 * NA * NI * NSTOK
      CALL RFILL (NW, 0.0, S)
      NW = 4 * MAXIF * MAXANT
      CALL RFILL (NW, 0.0, RMSS)
      NW = NA * NA * NI * NSTOK
      FVIS = 0
      SOLINT = MAX (-APARM(1), 0.01) / (24.0 * 3600.0)
      NREC = 0
      LSOU = -1
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      END = IRET.LT.0
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       Loop over buffer
      ELSE
         DOAVG = (IRET.LT.0) .OR. (RPARM(1+ILOCT).GT.TE)
         ISOU = 1
         IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
         DOAVG = DOAVG .OR. (ISOU.NE.LSOU)
C                                       finished average interval
         IF (DOAVG) THEN
C                                       not first
            IF (FVIS.GT.0) THEN
C                                       average data
               DO 130 JP = 1,NSTOK
                  DO 125 JI = 1,NI
                     DO 115 JA1 = 1,NA-1
                        DO 110 JA2 = JA1+1,NA
                           IF (AV(JA1,JA2,JI,JP).GT.0.0) THEN
                              AV(JA2,JA1,JI,JP) = AV(JA2,JA1,JI,JP) /
     *                           AV(JA1,JA2,JI,JP)
                              AV(JA1,JA2,JI,JP) = 0.0
                           ELSE
                              AV(JA2,JA1,JI,JP) = FBLANK
                              AV(JA1,JA2,JI,JP) = FBLANK
                              END IF
 110                       CONTINUE
 115                    CONTINUE
C                                       solve
                     CALL ASOLVE (0, ' ', 0., 0., NA, 8., AV(1,1,JI,JP),
     *                  SASQ, FRACT)
                     DO 120 JA1 = 1,NA
                        RMSS(JP,JI,JA1) = SASQ(JA1)
                        IF ((SASQ(JA1).GT.0.0) .AND.
     *                     (SASQ(JA1).NE.FBLANK)) THEN
                           S(1,JA1,JI,JP) = S(1,JA1,JI,JP) + SASQ(JA1)
                           S(2,JA1,JI,JP) = S(2,JA1,JI,JP) +
     *                        SASQ(JA1)**2
                           S(3,JA1,JI,JP) = S(3,JA1,JI,JP) + 1.0
                           END IF
 120                    CONTINUE
 125                 CONTINUE
 130              CONTINUE
C                                       write out
               NTIMES = NTIMES + 1
               RECORD(DATP(1,1)) = FVIS
               RECORD(DATP(2,1)) = NUMVIS
               RECR(DATP(4,1)) = TS / MAX (1, NT)
               DO 150 JA1 = 1,NA
                  RECORD(DATP(3,1)) = JA1
                  DO 145 JI = 1,NI
                     DO 140 JP = 1,NSTOK
                        JA2 = 4 * (JI-1) + JP
                        IF ((RMSS(JP,JI,JA1).LT.0.0) .OR.
     *                     (RMSS(JP,JI,JA1).EQ.FBLANK)) THEN
                           RECWTS(JA2) = 0.0
                        ELSE
                           RECWTS(JA2) = RMSS(JP,JI,JA1)
                           END IF
 140                    CONTINUE
 145                 CONTINUE
                  JA2 = 4 * NI
                  CALL RCOPY (JA2, RECWTS, RECR(DATP(5,1)))
                  NREC = NREC + 1
                  CALL TABIO ('WRIT', 0, NREC, RECORD, TABUFF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE'
                     GO TO 990
                     END IF
 150              CONTINUE
               END IF
C                                       leave if end
            TB = RPARM(1+ILOCT)
            TE = TB + SOLINT
            CALL RFILL (NW, 0.0, AV)
            NT = 0
            TS = 0.0
            FVIS = NUMVIS + 1
            LSOU = ISOU
            END IF
         IF (END) GO TO 190
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,100000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       solve for weights
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            JA1 = BASEN / 256. + 0.1
            JA2 = BASEN - JA1*256. + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         CALL RWAYWT (NSTOK, NCHAN, NIF, CHFLGS, RPARM, VIS, RMSS,
     *      COUNT)
         TS = TS + RPARM(1+ILOCT)
         NT = NT + 1
         DO 170 JP = 1,NSTOK
            DO 165 JI = 1,NI
               AV(JA1,JA2,JI,JP) = AV(JA1,JA2,JI,JP) + 1.0
               AV(JA2,JA1,JI,JP) = AV(JA2,JA1,JI,JP) + RMSS(JP,JI,1)
 165           CONTINUE
 170        CONTINUE
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       done first pass
 190  CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING TEMP TABLE'
         GO TO 990
         END IF
C                                       close uv data set
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       re-open WT table
      MSGTXT = 'Smooth the WT table'
      CALL MSGWRT (2)
      OPCODE = 'WRIT'
      CALL TABINI (OPCODE, 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPENING TEMP TABLE'
         GO TO 990
         END IF
C                                       time smooth
C                                       find averages and rms's
      SA(1) = 0.0
      SA(2) = 0.0
      SSA = 0.0
      DO 220 JP = 1,NSTOK
         DO 210 JI = 1,NI
            DO 200 JA = 1,NA
               IF (S(3,JA,JI,JP).GT.0.5) THEN
                  S(1,JA,JI,JP) = S(1,JA,JI,JP) / S(3,JA,JI,JP)
                  S(2,JA,JI,JP) = S(2,JA,JI,JP) / S(3,JA,JI,JP)
                  S(2,JA,JI,JP) = S(2,JA,JI,JP) - S(1,JA,JI,JP)**2
                  IF (S(2,JA,JI,JP).GT.0.0) THEN
                     W = 1.0 / S(2,JA,JI,JP)
                     SA(1) = SA(1) + W * S(1,JA,JI,JP)
                     SA(2) = SA(2) + W * S(1,JA,JI,JP) * S(1,JA,JI,JP)
                     SSA = SSA + W
                     END IF
                  S(2,JA,JI,JP) = SQRT (MAX(0., S(2,JA,JI,JP)))
                  END IF
 200           CONTINUE
 210        CONTINUE
 220     CONTINUE
      IF (SSA.GT.0.0) THEN
         SA(1) = SA(1) / SSA
         SA(2) = SA(2) / SSA - SA(1) * SA(1)
         SA(2) = SQRT (MAX (0.0, SA(2)))
         END IF
      IF (SA(2).LE.0.0) SA(2) = 1.E6
      NW1 = (NTIMES * 4 * NI + 6124) / 1024 + 1
      NW2 = (NTIMES * 3 + 6124) / 1024 + 1
      CALL ZMEMRY ('GET ', 'RWAYAN', NW1, WTSBUF, PWTS, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RWAYAN', NW2, WRKBUF, PWRK,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET,
     *      'GETTING DYNAMIC MEMORY FOR SMOOTHING'
         GO TO 990
         END IF
      CALL RWAYSM (DATP, NTIMES, NA, NI, SA, TABUFF, S, WTSBUF(1+PWTS),
     *   WRKBUF(1+PWRK), COUNT(4), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SMOOTHING THE TEMP TABLE'
         GO TO 999
         END IF
      CALL ZMEMRY ('FREE', 'RWAYAN', NW1, WTSBUF, PWTS, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('FREE', 'RWAYAN', NW2, WRKBUF,
     *   PWRK, IRET)
      NREC = 0
C                                       reopen to do output
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING OUTPUT VIS FILE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
      FVIS = 0
      NW = 4 * NIF
      MSGTXT = 'Apply the WT table'
      CALL MSGWRT (2)
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
 300  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-READING INPUT VIS FILE'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,100000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       read weights this interval
         IF (NUMVIS.GT.FVIS) THEN
            DO 310 JA1 = 1,NA
               NREC = NREC + 1
               CALL TABIO ('READ', 0, NREC, RECORD, TABUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READING TEMP TABLE'
                  GO TO 990
                  END IF
               JA2 = RECORD(DATP(3,1))
               CALL RCOPY (NW, RECR(DATP(5,1)), RMSS(1,1,JA2))
 310           CONTINUE
            FVIS = RECORD(DATP(2,1))
            END IF
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            JA1 = BASEN / 256. + 0.1
            JA2 = BASEN - JA1*256. + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       call user routine
         CALL RWAYAP (NSTOK, NCHAN, MAXIF, RMSS, JA1, JA2, RPARM, VIS,
     *      RESULT, GOTONE)
         IF (GOTONE) THEN
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT VIS FILE'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 300
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSHING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close WX table
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      XF = 100.0 * COUNT(2)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1310) XF
      CALL MSGWRT (4)
      XF = 100.0 * COUNT(3)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1311) XF
      CALL MSGWRT (4)
      WRITE (MSGTXT,1312) COUNT(4)
      IF (COUNT(4).GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT,1313) COUNT(5)
      IF (COUNT(5).GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT,1314) COUNT(6)
      IF (COUNT(6).GT.0) CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYAN: ERROR',I3,' ON ',A)
 1100 FORMAT ('RWAYAN: on visibility',I10)
 1310 FORMAT (F6.3,' % of spectra had Real/Imag rms > 1.5')
 1311 FORMAT (F6.3,' % of spectra had Imag/Real rms > 1.5')
 1312 FORMAT (I10,' spectra flagged due to smoothing and clipping')
 1313 FORMAT (I10,' spectra previously flagged')
 1314 FORMAT (I10,' spectra too few points')
      END
      SUBROUTINE RWAYWT (NS, NF, NI, CHFLGS, RPARM, VIS, RMSS, CNT)
C-----------------------------------------------------------------------
C   Inputs:
C      NS       I        # Stokes in data
C      NF       I        # spectral channels
C      NI       I        # IFs
C      CHFLGS   I        (NF,NIF) selection flags
C      VIS      R(3,*)   Visibilities in order real, imaginary, weight
C                        (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      INCSI    I        Input Stokes' increment in vis.
C      INCFI    I        Input frequency increment in vis.
C      INCIFI   I        Input IF increment in vis.
C   Output:
C      RMSS     R(4,*)   RMSes (POL, IF)
C-----------------------------------------------------------------------
      INTEGER   NS, NF, NI, CHFLGS(NF,NI), CNT(3)
      REAL      RPARM(*), VIS(3,*), RMSS(4,*)
C
      INTEGER   JIF, JF, JS, INDEXI, INDI, IT, L, LR, LI, LT
      REAL      RMSR, RMSI, WS(10), MEDIAN, WAIN
      DOUBLE PRECISION WT, SR, SSR, SI, SSI, WR, WI, AVR, AVI, V
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA WS /6.0, 5.0, 4.5, 4.3, 4.0, 3.8, 3.5, 3.2, 3.0, 3.0/
C-----------------------------------------------------------------------
C                                       robust method
      IF (OPTYPE.EQ.'RMSR') THEN
         DO 100 JIF = 1,NI
            DO 90 JS = 1,NS
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               AVR = 0.0D0
               AVI = 0.0D0
               RMSR = 1000.0
               RMSI = 1000.0
               DO 30 IT = 1,10
                  SR = 0.0D0
                  SSR = 0.0D0
                  WR = 0.0D0
                  LR = 0
                  SI = 0.0D0
                  SSI = 0.0D0
                  WI = 0.0D0
                  LI = 0
                  LT = 0
                  INDEXI = INDI
                  DO 20 JF = 1,NF
                     WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                     IF (WT.GT.0.0) THEN
                        LT = LT + 1
                        IF (NOWAY.GE.2) THEN
                           WT = 1.0D0
                        ELSE IF (NOWAY.EQ.1) THEN
                           WT = MAX (1.0, RPARM(1+ILOCIT))
                           END IF
                        V = VIS(1,INDEXI)
                        IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                           SR = SR + V * WT
                           SSR = SSR + V * V * WT
                           WR = WR + WT
                           LR = LR + 1
                           END IF
                        V = VIS(2,INDEXI)
                        IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                           SI = SI + V * WT
                           SSI = SSI + V * V * WT
                           WI = WI + WT
                           LI = LI + 1
                           END IF
                        END IF
                     INDEXI = INDEXI + INCFI
 20                  CONTINUE
                  IF (WR.GT.0.0) THEN
                     AVR = SR / WR
                     SSR = SSR / WR
                     RMSR = SSR - AVR * AVR
                     RMSR = SQRT (MAX (0.0, RMSR))
                     END IF
                  IF (WI.GT.0.0) THEN
                     AVI = SI / WI
                     SSI = SSI / WI
                     RMSI = SSI - AVI * AVI
                     RMSI = SQRT (MAX (0.0, RMSI))
                     END IF
 30               CONTINUE
C                                       check result
               WAIN = 1.0
               IF ((WR.LE.0.0D0) .AND. (WI.LE.0.0D0)) THEN
                  WT = 0.0D0
               ELSE IF (LT.LT.BPARM(1)) THEN
                  WR = 0.0D0
               ELSE
C                                       NOWAY not needed
                  WAIN = (WI + WR) / (LI + LR)
                  CNT(1) = CNT(1) + 1
                  IF ((WR.GT.0.0) .AND. (WI.LE.0.0)) THEN
                     CNT(3) = CNT(3) + 1
                     RMSI = RMSR
                  ELSE IF ((WR.LE.0.0) .AND. (WI.GT.0.0)) THEN
                     CNT(2) = CNT(2) + 1
                     RMSR = RMSI
                     END IF
                  RMSI = MAX (1.E-4, RMSI)
                  RMSR = MAX (1.E-4, RMSR)
                  WT = SQRT (RMSR * RMSI)
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                  END IF
C                                       apply result
               RMSS(JS,JIF) = WT * SQRT (WAIN)
 90            CONTINUE
 100        CONTINUE
C                                       median method
      ELSE IF (OPTYPE.EQ.'MEDI') THEN
         DO 200 JIF = 1,NI
            DO 190 JS = 1,NS
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               INDEXI = INDI
               L = 0
               WAIN = 0.0
               DO 110 JF = 1,NF
                  WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                  IF (WT.GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        WT = 1.0D0
                     ELSE IF (NOWAY.EQ.1) THEN
                        WT = MAX (1.0, RPARM(1+ILOCIT))
                        END IF
                     L = L + 1
                     WAIN = WAIN + WT
                     BUFFS(L,1) = VIS(1,INDEXI)
                     BUFFS(L,2) = VIS(2,INDEXI)
                     END IF
                  INDEXI = INDEXI + INCFI
 110              CONTINUE
               IF ((L.GT.0) .AND. (L.LT.BPARM(1))) THEN
                  WT = 0.0D0
               ELSE IF (L.GT.0) THEN
                  AVR = MEDIAN (L, BUFFS(1,1))
                  AVI = MEDIAN (L, BUFFS(1,2))
                  DO 120 IT = 1,L
                     BUFFS(IT,1) = ABS (BUFFS(IT,1) - AVR)
                     BUFFS(IT,2) = ABS (BUFFS(IT,2) - AVI)
 120                 CONTINUE
                  RMSR = 1.4826 * MEDIAN (L, BUFFS(1,1))
                  RMSI = 1.4826 * MEDIAN (L, BUFFS(1,2))
                  CNT(1) = CNT(1) + 1
                  IF ((WR.GT.0.0) .AND. (WI.LE.0.0)) THEN
                     CNT(3) = CNT(3) + 1
                     RMSI = RMSR
                  ELSE IF ((WR.LE.0.0) .AND. (WI.GT.0.0)) THEN
                     CNT(2) = CNT(2) + 1
                     RMSR = RMSI
                     END IF
                  RMSI = MAX (1.E-4, RMSI)
                  RMSR = MAX (1.E-4, RMSR)
                  WT = SQRT (RMSR * RMSI)
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                  WAIN = WAIN / L
                  IF (NOWAY.GE.2) WAIN = 1.0
C                                       all flagged
               ELSE
                  WT = 0.0D0
                  END IF
C                                       apply result
               RMSS(JS,JIF) = WT * SQRT (WAIN)
 190           CONTINUE
 200        CONTINUE
C                                       robust median
      ELSE
         DO 300 JIF = 1,NI
            DO 290 JS = 1,NS
               INDI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
               AVR = 0.0D0
               AVI = 0.0D0
               RMSR = 1000.0
               RMSI = 1000.0
               DO 240 IT = 1,10,2
                  LR = 0
                  LI = 0
                  LT = 0
                  INDEXI = INDI
                  WR = 0.0D0
                  WI = 0.0D0
                  DO 220 JF = 1,NF
                     WT = VIS(3,INDEXI) * CHFLGS(JF,JIF)
                     IF (WT.GT.0.0D0) THEN
                        LT = LT + 1
                        IF (NOWAY.GE.2) THEN
                           WT = 1.0D0
                        ELSE IF (NOWAY.EQ.1) THEN
                           WT = MAX (1.0, RPARM(1+ILOCIT))
                           END IF
                        V = VIS(1,INDEXI)
                        IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                           LR = LR + 1
                           BUFFS(LR,1) = V
                           WR = WR + WT
                           END IF
                        V = VIS(2,INDEXI)
                        IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                           LI = LI + 1
                           BUFFS(LI,2) = V
                           WI = WI + WT
                           END IF
                        END IF
                     INDEXI = INDEXI + INCFI
 220                 CONTINUE
                  IF (LR.GT.0.0) THEN
                     AVR = MEDIAN (LR, BUFFS(1,1))
                     DO 225 L = 1,LR
                        BUFFS(L,1) = ABS (BUFFS(L,1)-AVR)
 225                    CONTINUE
                     RMSR = 1.4826 * MEDIAN (LR, BUFFS(1,1))
                     END IF
                  IF (LI.GT.0.0) THEN
                     AVI = MEDIAN (LI, BUFFS(1,2))
                     DO 230 L = 1,LI
                        BUFFS(L,2) = ABS (BUFFS(L,2)-AVI)
 230                    CONTINUE
                     RMSI = 1.4826 * MEDIAN (LI, BUFFS(1,2))
                     END IF
 240              CONTINUE
C                                       check result
               WAIN = 1.0
               IF ((LR.LE.0) .AND. (LI.LE.0)) THEN
                  WT = 0.0D0
               ELSE IF (LT.LT.BPARM(1)) THEN
                  WT = 0.0D0
               ELSE
                  CNT(1) = CNT(1) + 1
                  IF ((LR.GT.0) .AND. (LI.LE.0)) THEN
                     CNT(3) = CNT(3) + 1
                     RMSI = RMSR
                  ELSE IF ((LR.LE.0) .AND. (LI.GT.0)) THEN
                     CNT(2) = CNT(2) + 1
                     RMSR = RMSI
                     END IF
                  RMSI = MAX (1.E-4, RMSI)
                  RMSR = MAX (1.E-4, RMSR)
                  WT = SQRT (RMSR * RMSI)
                  IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                  IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
C                                       NOWAY not needed
                  WAIN = (WR + WI) / (LR + LI)
                  END IF
C                                       apply result
               RMSS(JS,JIF) = WT * SQRT (WAIN)
 290           CONTINUE
 300        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWAYAP (NS, NF, NI, RMSS, JA1, JA2, RPARM, VIS, RESULT,
     *   GOTONE)
C-----------------------------------------------------------------------
C   Inputs:
C      NS       I       # Stokes in data
C      NF       I       # spectral channels
C      NI       I       # IFs
C      JA1      I       # of antenna 1
C      JA2      I       # of antenna 2
C      VIS      R(3,*)  Visibilities in order real, imaginary, weight
C                       (Jy, Jy, unitless).  Weight <= 0 => flagged.
C   Inputs from COMMON:
C      INCSI    I        Input Stokes' increment in vis.
C      INCFI    I        Input frequency increment in vis.
C      INCIFI   I        Input IF increment in vis.
C      LRECO    I        Output file record length
C      NRPRMO   I        Output number of random parameters.
C      INCSO    I        Output Stokes' increment in vis.
C      INCFO    I        Output frequency increment in vis.
C      INCIFO   I        Output IF increment in vis.
C   Output:
C      RESULT   R(3,*)   Output visibilities selected in frequency.
C      GOTONE   L        T => there is some valid data in RESULT
C-----------------------------------------------------------------------
      INTEGER   NS, NF, NI, JA1, JA2
      REAL      VIS(3,*), RMSS(4,NI,*), RESULT(3,*), RPARM(*)
      LOGICAL   GOTONE
C
      INTEGER   JIF, JS, JF, INDEXO, INDEXI, KP
      REAL      WT, W
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       apply result
      GOTONE = .FALSE.
      DO 100 JIF = 1,NIF
         DO 90 JS = 1,NS
            KP = (JS+1) / 2
            INDEXI = (JIF-1) * INCIFI + (JS-1) * INCSI + 1
            INDEXO = (JIF-1) * INCIFO + (JS-1) * INCSO + 1
            WT = RMSS(JS,JIF,JA1) * RMSS(JS,JIF,JA2)
            IF (WT.GT.0.0) THEN
               WT = 1.0 / (WT * WT)
               W = (1.0 / RMSS(JS,JIF,JA1))**4
               IF (NSUMS(JA1,JS,JIF,1).LE.0) THEN
                  MNWTS(JA1,JS,JIF,1) = W
                  MXWTS(JA1,JS,JIF,1) = W
                  SUMWTS(JA1,JS,JIF,1) = W
                  SUMSQW(JA1,JS,JIF,1) = W * W
                  NSUMS(JA1,JS,JIF,1) = 1
               ELSE
                  MNWTS(JA1,JS,JIF,1) = MIN (MNWTS(JA1,JS,JIF,1), W)
                  MXWTS(JA1,JS,JIF,1) = MAX (MXWTS(JA1,JS,JIF,1), W)
                  SUMWTS(JA1,JS,JIF,1) = SUMWTS(JA1,JS,JIF,1) + W
                  SUMSQW(JA1,JS,JIF,1) = SUMSQW(JA1,JS,JIF,1) + W * W
                  NSUMS(JA1,JS,JIF,1) = NSUMS(JA1,JS,JIF,1) + 1
                  END IF
               W = (1.0 / RMSS(JS,JIF,JA2))**4
               IF (NSUMS(JA2,JS,JIF,1).LE.0) THEN
                  MNWTS(JA2,JS,JIF,1) = W
                  MXWTS(JA2,JS,JIF,1) = W
                  SUMWTS(JA2,JS,JIF,1) = W
                  SUMSQW(JA2,JS,JIF,1) = W * W
                  NSUMS(JA2,JS,JIF,1) = 1
               ELSE
                  MNWTS(JA2,JS,JIF,1) = MIN (MNWTS(JA2,JS,JIF,1), W)
                  MXWTS(JA2,JS,JIF,1) = MAX (MXWTS(JA2,JS,JIF,1), W)
                  SUMWTS(JA2,JS,JIF,1) = SUMWTS(JA2,JS,JIF,1) + W
                  SUMSQW(JA2,JS,JIF,1) = SUMSQW(JA2,JS,JIF,1) + W * W
                  NSUMS(JA2,JS,JIF,1) = NSUMS(JA2,JS,JIF,1) + 1
                  END IF
               END IF
            DO 80 JF = 1,NF
               RESULT(1,INDEXO) = VIS(1,INDEXI)
               RESULT(2,INDEXO) = VIS(2,INDEXI)
               IF (VIS(3,INDEXI).GT.0.0) THEN
                  IF (NOWAY.GE.2) THEN
                     RESULT(3,INDEXO) = WT
                  ELSE IF (NOWAY.EQ.1) THEN
                     RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT)
                  ELSE
                     RESULT(3,INDEXO) = WT * VIS(3,INDEXI)
                     END IF
               ELSE
                  RESULT(3,INDEXO) = VIS(3,INDEXI)
                  END IF
               IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
               INDEXI = INDEXI + INCFI
               INDEXO = INDEXO + INCFO
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RWAYSM (DATP, NTIMES, NA, NI, SA, BUFFER, S, RMSS, WRK,
     *   CNT, IRET)
C-----------------------------------------------------------------------
C   REWASM smooths the weight table in time
C   Inputs:
C      DATP     I(128,2)   Table type & pointer
C      NTIMES   I          Number different times
C      NA       I          Number antennas
C      NI       I          Number IFs
C      SA       R(2)       Average and rms over all S
C      S        R(3,*)     Arrays of av, rms for NA,NI,NSTOK
C   Outputs:
C      BUFFER   I(512)     Table IO buffer already open
C      RMSS     R(*)       scratch
C      WRK      R(*)       scratch
C      IRET     I          Error code
C-----------------------------------------------------------------------
      INTEGER   DATP(128,2), NTIMES, NA, NI, BUFFER(512), CNT(2), IRET
      REAL      SA(2), S(3,NA,NI,*), RMSS(4,NI,*), WRK(3,*)
C
      INCLUDE 'REWAY.INC'
      INTEGER   RECORD(4+4*MAXIF), IANT, IREC, IT, NW, JI, JS, JT,
     *   FTYPE, K, FGBUFF(512), FGLUN, LFGRNO, FGKOLS(MAXFGC),
     *   FGNUMV(MAXFGC), IDATE(3), ITIME(3), FGERR
      REAL      W, RECR(4+4*MAXIF), SMFUNC, SIGMA(3), VL, VH, BTIME,
     *   ETIME
      CHARACTER REASON*24
      EQUIVALENCE (RECORD, RECR)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA FGLUN /78/
C-----------------------------------------------------------------------
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      WRITE (REASON,1010) IDATE, ITIME(1), ITIME(2)
      FGERR = 0
      IF (FGVERO.LT.0) FGERR = 1
      LFGRNO = 0
      NW = 4 * NI
      RMSMAX =  1.E10
      IF (APARM(7).GT.0.0) RMSMAX = 1.0 / SQRT (APARM(7))
      IF ((APARM(8).LE.0.0) .OR. (APARM(8).LE.APARM(7))) APARM(8)=1.E12
      RMSMIN = 1.0 / SQRT (APARM(8))
      W  = SA(1) - APARM(6)*SA(2)
      RMSMIN = MAX (RMSMIN, W)
      W  = SA(1) + APARM(6)*SA(2)
      RMSMAX = MIN (RMSMAX, W)
      FTYPE = APARM(4) + 0.1
      IF ((FTYPE.LE.0) .OR. (FTYPE.GT.2)) FTYPE = 0
      APARM(4) = FTYPE
      SIGMA(1) = APARM(3) / (2.0 * 24.0 * 3600.0)
      SIGMA(2) = 1.2 * SIGMA(1)
      SIGMA(3) = 1.4 * SIGMA(2)
      DO 5 K = 1,3
         IF (FTYPE.EQ.0) THEN
            SIGMA(K) = SIGMA(K) * SIGMA(K) / LOG (2.0)
         ELSE IF (FTYPE.EQ.1) THEN
            SIGMA(K) = SIGMA(K) / LOG (2.0)
            END IF
 5       CONTINUE
C                                       loop over antennas
      DO 100 IANT = 1,NA
         IREC = IANT
         DO 20 IT = 1,NTIMES
            CALL TABIO ('READ', 0, IREC, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING TEMP TABLE'
               GO TO 990
               END IF
            IREC = IREC + NA
            CALL RCOPY (NW, RECR(DATP(5,1)), RMSS(1,1,IT))
            WRK(1,IT) = RECR(DATP(4,1))
 20         CONTINUE
         DO 50 JI = 1,NI
            DO 45 JS = 1,NSTOK
               IF (S(2,IANT,JI,JS).GT.0.0) THEN
                  VL = S(1,IANT,JI,JS) - APARM(5)*S(2,IANT,JI,JS)
                  VL = MAX (VL, RMSMIN)
                  VH = S(1,IANT,JI,JS) + APARM(5)*S(2,IANT,JI,JS)
                  VH = MIN (VH, RMSMAX)
               ELSE
                  VL = RMSMIN
                  VH = RMSMAX
                  END IF
C                                       smooth to buffer
               DO 35 IT = 1,NTIMES
C                                       flagged on input
                  IF (RMSS(JS,JI,IT).LE.0.0) THEN
                     WRK(3,IT) = 0.0
                     CNT(2) = CNT(2) + 1
                     GO TO 35
                     END IF
                  DO 34 K = 1,3
                     IF ((RMSS(JS,JI,IT).GE.VL) .AND.
     *                  (RMSS(JS,JI,IT).LE.VH)) THEN
                        WRK(2,IT) = 1.0
                        WRK(3,IT) = RMSS(JS,JI,IT)
                     ELSE
                        WRK(2,IT) = 0.0
                        WRK(3,IT) = 0.0
                        END IF
                     JT = IT
 25                  JT = JT - 1
                     IF (JT.GT.0) THEN
                        IF ((RMSS(JS,JI,JT).GE.VL) .AND.
     *                     (RMSS(JS,JI,JT).LE.VH)) THEN
                           W = SMFUNC (FTYPE, SIGMA(K),
     *                        WRK(1,JT)-WRK(1,IT))
                           WRK(2,IT) = WRK(2,IT) + W
                           WRK(3,IT) = WRK(3,IT) + W * RMSS(JS,JI,JT)
                           IF (W.GT.0.001) GO TO 25
                        ELSE
                           GO TO 25
                           END IF
                        END IF
                     JT = IT
 30                  JT = JT + 1
                     IF (JT.LE.NTIMES) THEN
                        IF ((RMSS(JS,JI,JT).GE.VL) .AND.
     *                     (RMSS(JS,JI,JT).LE.VH)) THEN
                           W = SMFUNC (FTYPE, SIGMA(K),
     *                        WRK(1,JT)-WRK(1,IT))
                           WRK(2,IT) = WRK(2,IT) + W
                           WRK(3,IT) = WRK(3,IT) + W * RMSS(JS,JI,JT)
                           IF (W.GT.0.001) GO TO 30
                        ELSE
                           GO TO 30
                           END IF
                        END IF
                     IF (WRK(2,IT).GT.0.0) THEN
                        WRK(3,IT) = WRK(3,IT) / WRK(2,IT)
                        GO TO 35
                        END IF
 34                  CONTINUE
                  IF ((WRK(3,IT).LT.RMSMIN) .OR. (WRK(3,IT).GT.RMSMAX))
     *               THEN
                     WRK(3,IT) = -1.0
                     CNT(1) = CNT(1) + 1
                     END IF
 35               CONTINUE
C                                       put back in array
               BTIME = -999.
               DO 40 IT = 1,NTIMES
                  RMSS(JS,JI,IT) = WRK(3,IT)
                  IF (WRK(3,IT).LE.0.0) THEN
                     IF ((BTIME.LT.-100.) .AND. (WRK(3,IT).LT.0.0)) THEN
                        BTIME = WRK(1,IT)
                        ETIME = WRK(1,IT)
                     ELSE
                        ETIME = WRK(1,IT)
                        END IF
                  ELSE IF (BTIME.GE.-100.) THEN
                     CALL FLAGIT ('FLAG', FGLUN, DISKIN, OLDCNO, FGVERI,
     *                  FGVERO, LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL,
     *                  IANT, 0, BTIME, ETIME, JI, JS, REASON, CATUV,
     *                  FGBUFF, FGERR)
                     BTIME = -999.
                     END IF
 40               CONTINUE
               IF (BTIME.GE.-100.) CALL FLAGIT ('FLAG', FGLUN, DISKIN,
     *            OLDCNO, FGVERI, FGVERO, LFGRNO, FGKOLS, FGNUMV,
     *            SUBARR, FRQSEL, IANT, 0, BTIME, ETIME, JI, JS,
     *            REASON, CATUV, FGBUFF, FGERR)
 45            CONTINUE
 50         CONTINUE
         IREC = IANT
         DO 60 IT = 1,NTIMES
            CALL TABIO ('READ', 0, IREC, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-READING TEMP TABLE'
               GO TO 990
               END IF
            CALL RCOPY (NW, RMSS(1,1,IT), RECR(DATP(5,1)))
            CALL TABIO ('WRIT', 0, IREC, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'RE-WRITING TEMP TABLE'
               GO TO 990
               END IF
            IREC = IREC + NA
 60         CONTINUE
 100     CONTINUE
      LFGRNO = LFGRNO - 1
      WRITE (MSGTXT,1100) LFGRNO
      IF (LFGRNO.GT.0) CALL MSGWRT (4)
      CALL FLAGIT ('CLOS', FGLUN, DISKIN, OLDCNO, FGVERI, FGVERO,
     *   LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL, IANT, 0, BTIME, ETIME,
     *   JI, JS, REASON, CATUV, FGBUFF, FGERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYSM: ERROR',I3,' ON ',A)
 1010 FORMAT ('REWAY ',I4,'/',I2.2,'/',I2.2,I3.2,':',I2.2)
 1100 FORMAT ('RWAYSM: wrote',I8,' FG table records')
      END
      SUBROUTINE RWAYBL (NA, NI, S, IRET)
C-----------------------------------------------------------------------
C   RWAYBL sends uv data one point at a time to the rms finding routine
C   The weights are stored in a table.  This table is sorted, smoothed,
C   and re-sorted.  Then the data and table are re-read and the baseline
C   based weights applied to the data.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Inputs:
C      NA      I      Max antenna number
C      NI      I      Max IF in data
C   Output:
C      S       R(*)   work array
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NA, NI, IRET
      REAL      S(3,NA,NA,NI,*)
C
      INCLUDE 'REWAY.INC'
      CHARACTER OFILE*48, OPCODE*4, TITLE(4)*24, UNITS(4)*8
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET, FVIS, LUN,
     *   CHFLGS(MAXCIF), NKEY, NREC, NCOL, DATP(128,2), VER, NW, JF,
     *   TABUFF(512), RECORD(3+4*MAXIF), KEYSUB(2,2), JA1, JA2, JI, JP,
     *   NTIMES, NW1, NW2, I, NRBL(MAXANT,MAXANT), KEY(2,2), INDEXI,
     *   INDEXO, KP, KA1, KA2
      LOGICAL   T, F, END, GOTONE
      REAL      VIS(3,UVBFSS/3), RESULT(3,UVBFSS/3), RPARM(20), TB, TE,
     *   BASEN, XF, WRKBUF(2), WTSBUF(2), RMSS(4,MAXIF), FKEY(2,2), WT,
     *   RECR(3+4*MAXIF), TEPS, SA(2), SSA, W
      LONGINT   PWRK, PWTS
      DOUBLE PRECISION UVSCAL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
      EQUIVALENCE (RESULT, BUFF3)
      EQUIVALENCE (RECORD, RECR)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA TITLE /'VIS NUMBER','BASELINE','TIME','BL RMS'/
      DATA UNITS /' ', ' ', 'DAYS', ' '/
      DATA FKEY /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      TEPS = 0.1 / (24. * 3600.)
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, NIF, CHNSEL, CHFLGS)
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      NTIMES = 0
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
      I = NA * NA * NI * NSTOK * 3
      CALL RFILL (I, 0.0, S)
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       make special table
      MSGTXT = 'Build the WT table'
      CALL MSGWRT (2)
      NKEY = 0
      NREC = 1000
      NCOL = 4
      CALL FILL (256, 0, DATP)
      DATP(1,2) = 14
      DATP(2,2) = 14
      DATP(3,2) = 12
      DATP(4,2) = 10 * (4 * NIF) + 2
      VER = 1
      LUN = 77
      CALL TABINI ('WRIT', 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING TEMP TABLE'
         GO TO 990
         END IF
      DO 10 I = 1,NCOL
         CALL CHR2H (24, TITLE(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 3, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
         CALL CHR2H (8, UNITS(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
 10      CONTINUE
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      TB = -1000.0
      TE = TB
      NW = 4 * MAXIF * MAXANT
      CALL RFILL (NW, 0.0, RMSS)
      NW = NA * NA * NI * NSTOK
      FVIS = 0
      NREC = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      END = IRET.LT.0
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,100000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            JA1 = BASEN / 256. + 0.1
            JA2 = BASEN - JA1*256. + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
         CALL RWAYWT (NSTOK, NCHAN, NIF, CHFLGS, RPARM, VIS, RMSS,
     *      COUNT)
C                                       check results
         DO 120 JI = 1,NI
            DO 110 JP = 1,NSTOK
               IF (RMSS(JP,JI).GT.0.0) THEN
                  S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) + RMSS(JP,JI)
                  S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) +
     *               RMSS(JP,JI)**2
                  S(3,JA1,JA2,JI,JP) = S(3,JA1,JA2,JI,JP) + 1.0
                  END IF
 110           CONTINUE
 120        CONTINUE
C                                       have some result here
C                                       write out
         NRBL(JA1,JA2) = NRBL(JA1,JA2) + 1
         NTIMES = MAX (NTIMES, NRBL(JA1,JA2))
         RECORD(DATP(1,1)) = NUMVIS
         RECORD(DATP(2,1)) = 256 * JA1 + JA2
         RECR(DATP(3,1)) = RPARM(1+ILOCT)
         JA2 = 4 * NI
         CALL RCOPY (JA2, RMSS, RECR(DATP(4,1)))
         NREC = NREC + 1
         CALL TABIO ('WRIT', 0, NREC, RECORD, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE'
            GO TO 990
            END IF
C                                       Read next buffer.
         GO TO 100
         END IF
C                                       done first pass
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING TEMP TABLE'
         GO TO 990
         END IF
C                                       close uv data set
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       find averages and rmses
      DO 160 JA1 = 1,NA
         DO 150 JA2 = JA1,NA
            DO 140 JI = 1,NI
               DO 130 JP = 1,NSTOK
                  IF (S(3,JA1,JA2,JI,JP).GT.0.5) THEN
                     S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) /
     *                  S(3,JA1,JA2,JI,JP)
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) /
     *                  S(3,JA1,JA2,JI,JP)
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) -
     *                  S(1,JA1,JA2,JI,JP)**2
                     IF (S(2,JA1,JA2,JI,JP).GT.0.0) THEN
                        W = 1.0 / S(2,JA1,JA2,JI,JP)
                        SSA = SSA + W
                        SA(1) = SA(1) + W * S(1,JA1,JA2,JI,JP)
                        SA(2) = SA(2) + W * S(1,JA1,JA2,JI,JP) *
     *                     S(1,JA1,JA2,JI,JP)
                        END IF
                     S(2,JA1,JA2,JI,JP) = SQRT
     *                  (MAX (0.0, S(2,JA1,JA2,JI,JP)))
                     END IF
 130              CONTINUE
 140           CONTINUE
 150        CONTINUE
 160     CONTINUE
      IF (SSA.GT.0.0) THEN
         SA(1) = SA(1) / SSA
         SA(2) = SA(2) / SSA - SA(1) * SA(1)
         SA(2) = SQRT (MAX (0.0, SA(2)))
         END IF
      IF (SA(2).LE.0.0) SA(2) = 1.E6
C                                       sort WT table
      MSGTXT = 'Sort, smooth, re-sort the WT table'
      CALL MSGWRT (2)
      KEY(1,1) = 2
      KEY(2,1) = 0
      KEY(1,2) = 3
      KEY(2,2) = 0
      CALL TABSRT (DISKO, NEWCNO, 'WT', VER, VER, KEY, KEYSUB, FKEY,
     *   TABUFF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SORTING TEMP TABLE'
         GO TO 990
         END IF
C                                       re-open WT table
      OPCODE = 'WRIT'
      CALL TABINI (OPCODE, 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPENING TEMP TABLE'
         GO TO 990
         END IF
C                                       time smooth
      NW1 = (NTIMES * 4 * NI + 6124) / 1024 + 1
      NW2 = (NTIMES * 3 + 6124) / 1024 + 1
      CALL ZMEMRY ('GET ', 'RWAYBL', NW1, WTSBUF, PWTS, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RWAYBL', NW2, WRKBUF,
     *   PWRK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET,
     *      'GETTING DYNAMIC MEMORY FOR SMOOTHING'
         GO TO 990
         END IF
      CALL RWAYSC (DATP, NA, NI, SA, S, TABUFF, WTSBUF(1+PWTS),
     *   WRKBUF(1+PWRK), COUNT(4), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SMOOTHING THE TEMP TABLE'
         GO TO 990
         END IF
      CALL ZMEMRY ('FREE', 'RWAYBL', NW1, WTSBUF, PWTS, IRET)
      CALL ZMEMRY ('FREE', 'RWAYBL', NW2, WRKBUF, PWRK, IRET)
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING TEMP TABLE'
         GO TO 990
         END IF
C                                       sort WT table
      KEY(1,1) = 1
      KEY(2,1) = 0
      KEY(1,2) = 3
      KEY(2,2) = 0
      CALL TABSRT (DISKO, NEWCNO, 'WT', VER, VER, KEY, KEYSUB, FKEY,
     *   TABUFF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-SORTING TEMP TABLE'
         GO TO 990
         END IF
C                                       reopen to do output
      NREC = 0
C                                       re-open WT table
      OPCODE = 'READ'
      CALL TABINI (OPCODE, 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-RE-OPENING TEMP TABLE'
         GO TO 990
         END IF
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING OUTPUT VIS FILE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
      FVIS = 0
      NW = 4 * NI
      MSGTXT = 'Apply the WT table'
      CALL MSGWRT (2)
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
 200  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-READING INPUT VIS FILE'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,100000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       read weights this record
         NREC = NREC + 1
         CALL TABIO ('READ', 0, NREC, RECORD, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TEMP TABLE'
            GO TO 990
            END IF
         CALL RCOPY (NW, RECR(DATP(4,1)), RMSS)
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            JA1 = BASEN / 256. + 0.1
            JA2 = BASEN - JA1*256. + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       sanity check
         IF ((NUMVIS.NE.RECORD(DATP(1,1))) .OR.
     *      (256*JA1+JA2.NE.RECORD(DATP(2,1))) .OR.
     *      (ABS(RPARM(1+ILOCT)-RECR(DATP(3,1))).GT.TEPS)) THEN
            WRITE (MSGTXT,1200) NUMVIS, JA1, JA2, RPARM(1+ILOCT)
            CALL MSGWRT (6)
            WRITE (MSGTXT,1201) RECORD(DATP(1,1)), RECORD(DATP(2,1)),
     *         RECR(DATP(3,1))
            CALL MSGWRT (6)
            IRET = 8
            GO TO 999
            END IF
C                                       apply result
         GOTONE = .FALSE.
         DO 240 JI = 1,NI
            DO 230 JP = 1,NSTOK
               KP = (JP + 1) / 2
               IF (MOD(JP,2).EQ.0) THEN
                  KA1 = JA2
                  KA2 = JA1
               ELSE
                  KA1 = JA1
                  KA2 = JA2
                  END IF
               INDEXI = (JI-1) * INCIFI + (JP-1) * INCSI + 1
               INDEXO = (JI-1) * INCIFO + (JP-1) * INCSO + 1
               WT = RMSS(JP,JI)
               IF (WT.GT.0.0) THEN
                  WT = 1.0 / (WT * WT)
                  IF (NSUMS(KA1,KA2,JI,KP).LE.0) THEN
                     MNWTS(KA1,KA2,JI,KP) = WT
                     MXWTS(KA1,KA2,JI,KP) = WT
                     SUMWTS(KA1,KA2,JI,KP) = WT
                     SUMSQW(KA1,KA2,JI,KP) = WT * WT
                     NSUMS(KA1,KA2,JI,KP) = 1
                  ELSE
                     MNWTS(KA1,KA2,JI,KP) =
     *                  MIN (MNWTS(KA1,KA2,JI,KP),WT)
                     MXWTS(KA1,KA2,JI,KP) =
     *                  MAX (MXWTS(KA1,KA2,JI,KP),WT)
                     SUMWTS(KA1,KA2,JI,KP) = SUMWTS(KA1,KA2,JI,KP) +
     *                  WT
                     SUMSQW(KA1,KA2,JI,KP) = SUMSQW(KA1,KA2,JI,KP) +
     *                  WT * WT
                     NSUMS(KA1,KA2,JI,KP) = NSUMS(KA1,KA2,JI,KP) + 1
                     END IF
                  END IF
               DO 220 JF = 1,NCHAN
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        RESULT(3,INDEXO) = WT
                     ELSE IF (NOWAY.EQ.1) THEN
                        RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT)
                     ELSE
                        RESULT(3,INDEXO) = WT * VIS(3,INDEXI)
                        END IF
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
                  IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 220              CONTINUE
 230           CONTINUE
 240        CONTINUE
         IF (GOTONE) THEN
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT VIS FILE'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 200
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSHING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close WX table
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      XF = 100.0 * COUNT(2)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1240) XF
      CALL MSGWRT (4)
      XF = 100.0 * COUNT(3)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1241) XF
      CALL MSGWRT (4)
      WRITE (MSGTXT,1242) COUNT(4)
      IF (COUNT(4).GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT,1243) COUNT(5)
      IF (COUNT(5).GT.0) CALL MSGWRT (4)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYBL: ERROR',I3,' ON ',A)
 1100 FORMAT ('RWAYBL: on visibility',I10)
 1200 FORMAT ('DATA VIS, ANTS, T',I12,I3,I3,F10.6)
 1201 FORMAT ('TAB VIS, BL, T   ',I12,I6,F10.6,' MISMATCH')
 1240 FORMAT (F6.3,' % of spectra had Real/Imag rms > 1.5')
 1241 FORMAT (F6.3,' % of spectra had Imag/Real rms > 1.5')
 1242 FORMAT (I10,' spectra flagged due to smoothing and clipping')
 1243 FORMAT (I10,' spectra previously flagged')
      END
      SUBROUTINE RWAYSC (DATP, NA, NI, SA, S, BUFFER, RMSS, WRK, CNT,
     *   IRET)
C-----------------------------------------------------------------------
C   REWASM smooths the weight table in time
C   Outputs:
C      IRET   I   Error code
C-----------------------------------------------------------------------
      INTEGER   DATP(128,2), NA, NI, BUFFER(512), CNT(2), IRET
      REAL      SA(2), S(3,NA,NA,NI,*), RMSS(4,NI,*), WRK(3,*)
C
      INCLUDE 'REWAY.INC'
      INTEGER   RECORD(4+4*MAXIF), IREC, IT, NW, JI, JS, JT, NREC, BREC,
     *   NT, JA1, JA2, CBL, IBL, FTYPE, K, FGBUFF(512), FGLUN, LFGRNO,
     *   FGKOLS(MAXFGC), FGNUMV(MAXFGC), IDATE(3), ITIME(3), FGERR
      REAL     W, SIGMA(3), RECR(4+4*MAXIF), VL, VH, SMFUNC, BTIME,
     *   ETIME
      CHARACTER REASON*24
      EQUIVALENCE (RECORD, RECR)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      DATA FGLUN /78/
C-----------------------------------------------------------------------
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      WRITE (REASON,1010) IDATE, ITIME(1), ITIME(2)
      FGERR = 0
      IF (FGVERO.LT.0) FGERR = 1
      LFGRNO = 0
      CNT(1) = 0
      CNT(2) = 0
      RMSMAX =  1.E10
      IF (APARM(7).GT.0.0) RMSMAX = 1.0 / SQRT (APARM(7))
      IF ((APARM(8).LE.0.0) .OR. (APARM(8).LE.APARM(7))) APARM(8)=1.E12
      RMSMIN = 1.0 / SQRT (APARM(8))
      W = SA(1) - APARM(6) * SA(2)
      RMSMIN = MAX (RMSMIN, W)
      W = SA(1) + APARM(6) * SA(2)
      RMSMAX = MIN (RMSMAX, W)
      NW = 4 * NI
      NREC = BUFFER(5)
      FTYPE = APARM(4) + 0.1
      IF ((FTYPE.LE.0) .OR. (FTYPE.GT.2)) FTYPE = 0
      APARM(4) = FTYPE
      SIGMA(1) = APARM(3) / (2.0 * 24.0 * 3600.0)
      SIGMA(2) = 1.2 * SIGMA(1)
      SIGMA(3) = 1.4 * SIGMA(2)
      DO 5 K = 1,3
         IF (FTYPE.EQ.0) THEN
            SIGMA(K) = SIGMA(K) * SIGMA(K) / LOG (2.0)
         ELSE IF (FTYPE.EQ.1) THEN
            SIGMA(K) = SIGMA(K) / LOG (2.0)
            END IF
 5       CONTINUE
      IREC = 0
      CBL = 0
      IT = 0
C                                       loop over baselines
 10   IREC = IREC + 1
      IF (IREC.LE.NREC) THEN
         CALL TABIO ('READ', 0, IREC, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING TEMP TABLE'
            GO TO 990
            END IF
         IBL = RECORD(DATP(2,1))
         IF ((IT.EQ.0) .OR. (IBL.EQ.CBL)) THEN
            IF (IT.EQ.0) BREC = IREC
            IT = IT + 1
            CBL = IBL
            CALL RCOPY (NW, RECR(DATP(4,1)), RMSS(1,1,IT))
            WRK(1,IT) = RECR(DATP(3,1))
            GO TO 10
            END IF
         END IF
      IREC = IREC -1
      NT = IT
      JA1 = CBL / 256
      JA2 = CBL - 256 * JA1
      DO 50 JI = 1,NI
         DO 45 JS = 1,NSTOK
            IF (S(2,JA1,JA2,JI,JS).GT.0.0) THEN
               VL = S(1,JA1,JA2,JI,JS) - APARM(5) * S(2,JA1,JA2,JI,JS)
               VL = MAX (VL, RMSMIN)
               VH = S(1,JA1,JA2,JI,JS) + APARM(5) * S(2,JA1,JA2,JI,JS)
               VH = MIN (VH, RMSMAX)
            ELSE
               VL = RMSMIN
               VH = RMSMAX
               END IF
C                                       smooth to buffer
            DO 35 IT = 1,NT
C                                       flagged om input
               IF (RMSS(JS,JI,IT).LE.0.0) THEN
                  WRK(3,IT) = 0.0
                  CNT(2) = CNT(2) + 1
                  GO TO 35
                  END IF
               DO 34 K = 1,3
                  IF ((RMSS(JS,JI,IT).GE.VL) .AND.
     *               (RMSS(JS,JI,IT).LE.VH)) THEN
                     WRK(2,IT) = 1.0
                     WRK(3,IT) = RMSS(JS,JI,IT)
                  ELSE
                     WRK(2,IT) = 0.0
                     WRK(3,IT) = 0.0
                     END IF
                  JT = IT
 25               JT = JT - 1
                  IF (JT.GT.0) THEN
                     IF ((RMSS(JS,JI,JT).GE.VL) .AND.
     *                  (RMSS(JS,JI,JT).LE.VH))THEN
                        W = SMFUNC (FTYPE, SIGMA(K),
     *                     WRK(1,JT)-WRK(1,IT))
                        WRK(2,IT) = WRK(2,IT) + W
                        WRK(3,IT) = WRK(3,IT) + W * RMSS(JS,JI,JT)
                        IF (W.GT.0.001) GO TO 25
                     ELSE
                        GO TO 25
                        END IF
                     END IF
                  JT = IT
 30               JT = JT + 1
                  IF (JT.LE.NT) THEN
                     IF ((RMSS(JS,JI,JT).GE.VL) .AND.
     *                  (RMSS(JS,JI,JT).LE.VH)) THEN
                        W = SMFUNC (FTYPE, SIGMA(K),
     *                     WRK(1,JT)-WRK(1,IT))
                        WRK(2,IT) = WRK(2,IT) + W
                        WRK(3,IT) = WRK(3,IT) + W * RMSS(JS,JI,JT)
                        IF (W.GT.0.001) GO TO 30
                     ELSE
                        GO TO 30
                        END IF
                     END IF
                  IF (WRK(2,IT).GT.0.0) THEN
                     WRK(3,IT) = WRK(3,IT) / WRK(2,IT)
                     GO TO 35
                     END IF
 34               CONTINUE
               IF ((WRK(3,IT).LT.RMSMIN) .OR. (WRK(3,IT).GT.RMSMAX))
     *            THEN
                  WRK(3,IT) = -1.0
                  CNT(1) = CNT(1) + 1
                  END IF
 35            CONTINUE
C                                       put back in array
            BTIME = -999.
            DO 40 IT = 1,NT
               RMSS(JS,JI,IT) = WRK(3,IT)
               IF (WRK(3,IT).LE.0.0) THEN
                  IF ((BTIME.LT.-100.) .AND. (WRK(3,IT).LT.0.0)) THEN
                     BTIME = WRK(1,IT)
                     ETIME = WRK(1,IT)
                  ELSE
                     ETIME = WRK(1,IT)
                     END IF
               ELSE IF (BTIME.GE.-100.) THEN
                  CALL FLAGIT ('FLAG', FGLUN, DISKIN, OLDCNO, FGVERI,
     *               FGVERO, LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL,
     *               JA1, JA2, BTIME, ETIME, JI, JS, REASON, CATUV,
     *               FGBUFF, FGERR)
                  BTIME = -999.
                  END IF
 40            CONTINUE
            IF (BTIME.GE.-100.) CALL FLAGIT ('FLAG', FGLUN, DISKIN,
     *         OLDCNO, FGVERI, FGVERO, LFGRNO, FGKOLS, FGNUMV, SUBARR,
     *         FRQSEL, JA1, JA2, BTIME, ETIME, JI, JS, REASON, CATUV,
     *         FGBUFF, FGERR)
 45         CONTINUE
 50      CONTINUE
      IREC = BREC - 1
      DO 60 IT = 1,NT
         IREC = IREC + 1
         CALL TABIO ('READ', 0, IREC, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-READING TEMP TABLE'
            GO TO 990
            END IF
         CALL RCOPY (NW, RMSS(1,1,IT), RECR(DATP(4,1)))
         CALL TABIO ('WRIT', 0, IREC, RECORD, BUFFER, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'RE-WRITING TEMP TABLE'
            GO TO 990
            END IF
 60      CONTINUE
      IF (IREC.LT.NREC) THEN
         IT = 0
         CBL = 0
         GO TO 10
         END IF
      LFGRNO = LFGRNO -1
      WRITE (MSGTXT,1060) LFGRNO
      IF (LFGRNO.GT.0) CALL MSGWRT (4)
      CALL FLAGIT ('CLOS', FGLUN, DISKIN, OLDCNO, FGVERI, FGVERO,
     *   LFGRNO, FGKOLS, FGNUMV, SUBARR, FRQSEL, JA1, JA2, BTIME, ETIME,
     *   JI, JS, REASON, CATUV, FGBUFF, FGERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYSC: ERROR',I3,' ON ',A)
 1010 FORMAT ('REWAY ',I4,'/',I2.2,'/',I2.2,I3.2,':',I2.2)
 1060 FORMAT ('RWAYSC: wrote',I8,' FG table records')
      END
      REAL FUNCTION SMFUNC (FTYPE, SIGMA, T)
C-----------------------------------------------------------------------
C   Evaluates a smoothing function
C   Inputs
C      FTYPE   I   0 Gaussian, 1 exp, 2 boxcar
C      SIGMA   R   half width function
C      T       R   argument
C   Outputs
C      SMFUNC  R   smoothing function value
C-----------------------------------------------------------------------
      INTEGER   FTYPE
      REAL      SIGMA, T
C
      REAL      X
C-----------------------------------------------------------------------
      X = ABS (T)
      IF (FTYPE.EQ.0) THEN
         SMFUNC = EXP (- T * T / SIGMA)
      ELSE IF (FTYPE.EQ.1) THEN
         SMFUNC = EXP (- X / SIGMA)
      ELSE
         SMFUNC = 1.0
         IF (X.GT.SIGMA) SMFUNC = 0.0
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B)
C-----------------------------------------------------------------------
C   moves the vis array to the right place in buffer
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NB, NI, CT
      REAL      RPARM(*), VIS(3,*), B(3,NC,NT,NB,NI,*)
C
      INTEGER   JA1, JA2, JBL, JI, JP, JF, INDI
      REAL      BASEN, WT
      INCLUDE 'REWAY.INC'
      INCLUDE 'INCS:DUVH.INC'
C-----------------------------------------------------------------------
      IF (ILOCB.GE.0) THEN
         BASEN = RPARM(1+ILOCB)
         JA1 = BASEN / 256. + 0.1
         JA2 = BASEN - JA1*256. + 0.1
      ELSE
         JA1 = RPARM(1+ILOCA1) + 0.1
         JA2 = RPARM(1+ILOCA2) + 0.1
         END IF
      JBL = NANT * (JA1-1) - ((JA1*(JA1-1))/2) + JA2
      DO 40 JI = 1,NI
         DO 30 JP = 1,NSTOK
            INDI = (JI-1) * INCIFI + (JP-1) * INCSI + 1 - INCFI
            DO 20 JF = 1,NCHAN
               INDI = INDI + INCFI
               B(1,JF,CT,JBL,JI,JP) = VIS(1,INDI)
               B(2,JF,CT,JBL,JI,JP) = VIS(2,INDI)
               WT = VIS(3,INDI)
               IF (WT.GT.0.0) THEN
                  IF (NOWAY.GE.2) THEN
                     WT = 1.0
                  ELSE IF (NOWAY.EQ.1) THEN
                     WT = RPARM(1+ILOCIT)
                     END IF
                  END IF
               B(3,JF,CT,JBL,JI,JP) = WT
 20            CONTINUE
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
      SUBROUTINE RWRMSF (NC, NT, NA, NB, NI, NCT, CHFLGS, B, AV, S,
     *   NVM, VM, CNT)
C-----------------------------------------------------------------------
C   robust rms in arrays
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, NCT, CHFLGS(NC,NI), NVM, CNT(6)
      REAL      B(3,NC,NT,NB,NI,*), S(3,NA,NA,NI,*), AV(4,NI,NA,NA),
     *   VM(NVM,2)
C
      INCLUDE 'REWAY.INC'
      INTEGER   JF, JI, JP, JA1, JA2, JT, IT, JB, L, LR, LI, LT
      REAL      RMSR, RMSI, WS(7), MEDIAN, WAIN
      DOUBLE PRECISION WT, SR, SSR, SI, SSI, WR, WI, AVR, AVI, V
      DATA WS /6.0, 5.0, 4.0, 3.6, 3.2, 3.0, 3.0/
C-----------------------------------------------------------------------
C                                       robust means
      IF (OPTYPE.EQ.'RMSR') THEN
         DO 100 JI = 1,NI
            DO 90 JP = 1,NSTOK
               DO 80 JA1 = 1,NA
                  DO 70 JA2 = JA1,NA
                     JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                     AVR = 0.0D0
                     AVI = 0.0D0
                     RMSR = 1000.0
                     RMSI = 1000.0
                     DO 30 IT = 1,7
                        SR = 0.0D0
                        SSR = 0.0D0
                        WR = 0.0D0
                        LR = 0
                        SI = 0.0D0
                        SSI = 0.0D0
                        WI = 0.0D0
                        LI = 0
                        LT = 0
                        DO 20 JF = 1,NC
                           IF (CHFLGS(JF,JI).GT.0) THEN
                              DO 10 JT = 1,NCT
                                 WT = B(3,JF,JT,JB,JI,JP)
                                 IF (WT.GT.0.0) THEN
                                    LT = LT + 1
                                    V = B(1,JF,JT,JB,JI,JP)
                                    IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                                       SR = SR + V * WT
                                       SSR = SSR + V * V * WT
                                       WR = WR + WT
                                       LR = LR + 1
                                       END IF
                                    V = B(2,JF,JT,JB,JI,JP)
                                    IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                                       SI = SI + V * WT
                                       SSI = SSI + V * V * WT
                                       WI = WI + WT
                                       LI = LI + 1
                                       END IF
                                    END IF
 10                              CONTINUE
                              END IF
 20                        CONTINUE
C                                       bail if none
                        IF ((WR.EQ.0.0D0) .AND. (WI.EQ.0.0D0)) GO TO 35
                        IF (LT.LT.BPARM(1)) GO TO 35
                        IF (WR.GT.0.0D0) THEN
                           AVR = SR / WR
                           SSR = SSR / WR
                           RMSR = SSR - AVR * AVR
                           RMSR = SQRT (MAX (0.0, RMSR))
                           END IF
                        IF (WI.GT.0.0D0) THEN
                           AVI = SI / WI
                           SSI = SSI / WI
                           RMSI = SSI - AVI * AVI
                           RMSI = SQRT (MAX (0.0, RMSI))
                           END IF
 30                     CONTINUE
C                                       check result
 35                  WAIN = 1.0
                     IF ((WR.LE.0.0D0) .AND. (WI.LE.0.0D0)) THEN
                        WT = 0.0D0
                        CNT(5) = CNT(5) + 1
                     ELSE IF (LT.LT.BPARM(1)) THEN
                        WT = -1.0D0
                        CNT(6) = CNT(6) + 1
                     ELSE
C                                       NOWAY not needed
                        WAIN = (WR + WI) / (LR + LI)
                        CNT(1) = CNT(1) + 1
                        IF ((WR.GT.0.0) .AND. (WI.LE.0.0)) THEN
                           CNT(3) = CNT(3) + 1
                           RMSI = RMSR
                        ELSE IF ((WR.LE.0.0) .AND. (WI.GT.0.0)) THEN
                           CNT(2) = CNT(2) + 1
                           RMSR = RMSI
                           END IF
                        RMSI = MAX (1.E-4, RMSI)
                        RMSR = MAX (1.E-4, RMSR)
                        WT = SQRT (RMSR * RMSI)
                        IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                        IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                        END IF
                     WT = WT * SQRT (WAIN)
                     AV(JP,JI,JA1,JA2) = WT
                     S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) + WT
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) + WT * WT
                     S(3,JA1,JA2,JI,JP) = S(3,JA1,JA2,JI,JP) + 1.0
 70                  CONTINUE
 80               CONTINUE
 90            CONTINUE
 100        CONTINUE
C                                       median method
      ELSE IF (OPTYPE.EQ.'MEDI') THEN
         DO 200 JI = 1,NI
            DO 190 JP = 1,NSTOK
               DO 180 JA1 = 1,NA
                  DO 170 JA2 = JA1,NA
                     JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                     L = 0
                     WR = 0.0D0
                     DO 120 JF = 1,NC
                        IF (CHFLGS(JF,JI).GT.0) THEN
                           DO 110 JT = 1,NCT
                              WT = B(3,JF,JT,JB,JI,JP)
                              IF (WT.GT.0.0) THEN
                                 L = L + 1
                                 WR = WR + WT
                                 VM(L,1) = B(1,JF,JT,JB,JI,JP)
                                 VM(L,2) = B(2,JF,JT,JB,JI,JP)
                                 END IF
 110                          CONTINUE
                           END IF
 120                    CONTINUE
                     WAIN = 1.0
                     IF ((L.GT.0) .AND. (L.LT.BPARM(1))) THEN
                        WT = -1.0D0
                        CNT(6) = CNT(6) + 1
                     ELSE IF (L.GT.0) THEN
                        IF (NOWAY.LE.1) WAIN = WR / L
                        AVR = MEDIAN (L, VM(1,1))
                        AVI = MEDIAN (L, VM(1,2))
                        DO 130 IT = 1,L
                           VM(IT,1) = ABS (VM(IT,1) - AVR)
                           VM(IT,2) = ABS (VM(IT,2) - AVI)
 130                       CONTINUE
                        RMSR = 1.4826 * MEDIAN (L, VM(1,1))
                        RMSI = 1.4826 * MEDIAN (L, VM(1,2))
                        CNT(1) = CNT(1) + 1
                        RMSI = MAX (1.E-4, RMSI)
                        RMSR = MAX (1.E-4, RMSR)
                        WT = SQRT (RMSR * RMSI)
                        IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                        IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                     ELSE
                        WT = 0.0D0
                        CNT(5) = CNT(5) + 1
                        END IF
                     WT = WT * SQRT (WAIN)
                     AV(JP,JI,JA1,JA2) = WT
                     S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) + WT
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) + WT * WT
                     S(3,JA1,JA2,JI,JP) = S(3,JA1,JA2,JI,JP) + 1.0
 170                 CONTINUE
 180              CONTINUE
 190           CONTINUE
 200        CONTINUE
C                                       robust median
      ELSE
         DO 300 JI = 1,NI
            DO 290 JP = 1,NSTOK
               DO 280 JA1 = 1,NA
                  DO 270 JA2 = JA1,NA
                     JB = NA * (JA1-1) - (JA1*(JA1-1))/2 + JA2
                     AVR = 0.0D0
                     AVI = 0.0D0
                     RMSR = 1000.0
                     RMSI = 1000.0
                     DO 240 IT = 1,7,2
                        LR = 0
                        LI = 0
                        LT = 0
                        WR = 0.0D0
                        WI = 0.0D0
                        DO 220 JF = 1,NC
                           IF (CHFLGS(JF,JI).GT.0) THEN
                              DO 210 JT = 1,NCT
                                 WT = B(3,JF,JT,JB,JI,JP)
                                 IF (WT.GT.0.0) THEN
                                    LT = LT + 1
                                    V = B(1,JF,JT,JB,JI,JP)
                                    IF (ABS(V-AVR).LT.WS(IT)*RMSR) THEN
                                       LR = LR + 1
                                       VM(LR,1)= B(1,JF,JT,JB,JI,JP)
                                       WR = WR + WT
                                       END IF
                                    V = B(2,JF,JT,JB,JI,JP)
                                    IF (ABS(V-AVI).LT.WS(IT)*RMSI) THEN
                                       LI = LI + 1
                                       VM(LI,2)= B(2,JF,JT,JB,JI,JP)
                                       WI = WI + WT
                                       END IF
                                    END IF
 210                             CONTINUE
                              END IF
 220                       CONTINUE
C                                       bail if none
                        IF ((LR.LE.0) .AND. (LI.LE.0)) GO TO 250
                        IF (LT.LT.BPARM(1)) GO TO 250
                        IF (LR.GT.0) THEN
                           AVR = MEDIAN (LR, VM(1,1))
                           DO 225 L = 1,LR
                              VM(L,1) = ABS (VM(L,1)-AVR)
 225                          CONTINUE
                           RMSR = 1.4826 * MEDIAN (LR, VM(1,1))
                           END IF
                        IF (LI.GT.0.0) THEN
                           AVI = MEDIAN (LI, VM(1,2))
                           DO 230 L = 1,LI
                              VM(L,2) = ABS (VM(L,2)-AVI)
 230                          CONTINUE
                           RMSI = 1.4826 * MEDIAN (LI, VM(1,2))
                           END IF
 240                    CONTINUE
C                                       check result
 250                 WAIN = 1.0
                     IF ((LR.LE.0) .AND. (LI.LE.0)) THEN
                        WT = -1.0D0
                        CNT(5) = CNT(5) + 1
                     ELSE IF (LT.LT.BPARM(1)) THEN
                        WT = 0.0D0
                        CNT(6) = CNT(6) + 1
                     ELSE
                        IF (NOWAY.LE.1) WAIN = (WR + WI) / (LR + LI)
                        CNT(1) = CNT(1) + 1
                        IF ((LR.GT.0) .AND. (LI.LE.0)) THEN
                           CNT(3) = CNT(3) + 1
                           RMSI = RMSR
                        ELSE IF ((LR.LE.0) .AND. (LI.GT.0)) THEN
                           CNT(2) = CNT(2) + 1
                           RMSR = RMSI
                           END IF
                        RMSI = MAX (1.E-4, RMSI)
                        RMSR = MAX (1.E-4, RMSR)
                        WT = SQRT (RMSR * RMSI)
                        IF (RMSR/RMSI.GT.1.5) CNT(2) = CNT(2) + 1
                        IF (RMSI/RMSR.GT.1.5) CNT(3) = CNT(3) + 1
                        END IF
                     WT = WT * SQRT (WAIN)
                     AV(JP,JI,JA1,JA2) = WT
                     S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) + WT
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) + WT * WT
                     S(3,JA1,JA2,JI,JP) = S(3,JA1,JA2,JI,JP) + 1.0
 270                 CONTINUE
 280              CONTINUE
 290           CONTINUE
 300        CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE RWAYFB (NC, NT, NA, NB, NI, B, S, AV, VM, IRET)
C-----------------------------------------------------------------------
C   RWAYFB accumulates a buffer of NT times for all baselines, etc.
C   It then finds the rms in the buffer for each baseline, stokes, IF
C   and saves the BL based rmses then in a WT table.  This table may
C   be sorted, smoothed, and re-sorted.  Then the data and table are
C   re-read and the rmses -> weights applied to the data.
C   Input in common:
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C   Inputs:
C      NC      I      Number spectral channels
C      NT      I      Number times to accumulate
C      NB      I      Max baseline number
C      NI      I      Max IF in data
C   Output:
C      B       R(*)   Big buffer to accumulate the vis
C      S       R(*)   work array
C      AV      R(*)   work array
C      IRET    I      Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   NC, NT, NA, NB, NI, IRET
      REAL      B(3,NC,NT,NB,NI,*), S(3,NA,NA,NI,*), AV(4,NI,NA,NA),
     *   VM(NC,NT,2)
C
      INTEGER   NTMAX
      PARAMETER (NTMAX = 99)
      INCLUDE 'REWAY.INC'
      CHARACTER OFILE*48, OPCODE*4, TITLE(4)*24, UNITS(4)*8
      INTEGER   IPTRO, LUNO, INDO, ILENBU, KBIND, NIOUT, NIOLIM, BO, VO,
     *   NUMVIS, XCOUNT, NCORO, NCOPY, CATMP(256), RNXRET, FVIS,
     *   CHFLGS(MAXCIF), LUN, NKEY, NREC, NCOL, DATP(128,2), VER,
     *   TABUFF(512), RECORD(4+4*MAXIF), JA1, JA2, JI, JP, NW, NVM,
     *   NTIMES, NW1, NW2, I, KEY(2,2), INDEXI, INDEXO, JF, TNUM(NTMAX),
     *   VISN(2,NTMAX), NTNUM, LSOU, ISOU, NCT, CT, J, J1, J2, JT,
     *   NRBL(MAXANT,MAXANT), KEYSUB(2,2), MAXREC, KA1, KA2, KP
      LOGICAL   T, F, END, FIRST, GOTONE
      REAL      VIS(3,UVBFSS/3), RESULT(3,UVBFSS/3), RPARM(20), TB, TE,
     *   BASEN, XF, WRKBUF(2), WTSBUF(2), FKEY(2,2), WT, TEPS, TIME,
     *   RECR(4+4*MAXIF), TIMES(NTMAX), TLIMIT, SA(2), SSA, W
      LONGINT   PWRK, PWTS
      DOUBLE PRECISION UVSCAL
      HOLLERITH HOLTMP(6)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (VIS, BUFF1)
      EQUIVALENCE (RESULT, BUFF3)
      EQUIVALENCE (RECORD, RECR)
      DATA LUNO /17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
      DATA TITLE /'VIS NUMBER','BASELINE','TIME','BL RMS'/
      DATA UNITS /' ', ' ', 'DAYS', ' '/
      DATA FKEY /1.0, 0.0, 1.0, 0.0/
      DATA KEYSUB /4*1/
C-----------------------------------------------------------------------
      NVM = NC * NT
      TEPS = 0.1 / (24. * 3600.)
      IF (APARM(2).LE.0.0) APARM(2) = 600.
      TLIMIT = APARM(2) / (24. * 3600.)
C                                       counters, mask
      NCHAN = CATBLK(KINAX+JLOCF)
      CALL CHWANT (NCHAN, NIF, CHNSEL, CHFLGS)
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      COUNT(6) = 0
      NTIMES = 0
      I = NA * NA * NI * NSTOK * 3
      CALL RFILL (I, 0.0, S)
      I = MAXANT * MAXANT
      CALL FILL (I, 0, NRBL)
C                                       Number of visibilities in input
C                                       and output files.
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       make special table
      NKEY = 0
      NREC = 1000
      NCOL = 4
      CALL FILL (256, 0, DATP)
      DATP(1,2) = 24
      DATP(2,2) = 14
      DATP(3,2) = 12
      DATP(4,2) = 10 * (4 * NIF) + 2
      VER = 1
      LUN = 77
      CALL TABINI ('WRIT', 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING TEMP TABLE'
         GO TO 990
         END IF
      DO 10 I = 1,NCOL
         CALL CHR2H (24, TITLE(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 3, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
         CALL CHR2H (8, UNITS(I), 1, HOLTMP)
         CALL TABIO ('WRIT', 4, I, HOLTMP, TABUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE COLUMN LABELS'
            GO TO 990
            END IF
 10      CONTINUE
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      NUMVIS = 0
      TB = -1000.0
      TE = TB
      NW = 3 * NC * NT * NB * NI * NSTOK
      CALL RFILL (NW, 0.0, B)
      FVIS = 0
      NREC = 0
      NTNUM = 0
      LSOU = -1
      CT = 0
      NCT = 0
      FIRST = .TRUE.
      GOTONE = .FALSE.
      MSGTXT = 'Build the WT table'
      CALL MSGWRT (2)
C                                       Loop
C                                       Read vis. record.
 100  CALL UVGET ('READ', RPARM, VIS, IRET)
      END = IRET.LT.0
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING DATA FIRST PASS'
         GO TO 990
C                                       got good data, now what
      ELSE IF (IRET.LE.0) THEN
C                                       in current time
         IF (END) THEN
            TIME = 1.E4
         ELSE
            TIME = RPARM(1+ILOCT)
            NUMVIS = NUMVIS + 1
            END IF
         IF (MOD(NUMVIS,50000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
         ISOU = 1
         IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU) + 0.01
         IF (END) ISOU = -1
C                                       in current time/source
         IF ((ABS(TB-TIME).LE.TEPS) .AND. (ISOU.EQ.LSOU)) THEN
            VISN(2,CT) = NUMVIS
            CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B)
            GOTONE = .TRUE.
C                                       just advance time counter
         ELSE IF ((NCT.LT.NT) .AND. (ISOU.EQ.LSOU) .AND.
     *      (TIME-TIMES(TNUM(1)).LT.TLIMIT)) THEN
            NCT = NCT + 1
            TNUM(NCT) = NCT
            CT = NCT
            TIMES(NCT) = TIME
            VISN(1,CT) = NUMVIS
            VISN(2,CT) = NUMVIS
            CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B)
            GOTONE = .TRUE.
            TB = TIME
C                                       still going, clear 1 or more
         ELSE IF ((ISOU.EQ.LSOU) .AND. (TIME-TIMES(TNUM(1)).LT.TLIMIT))
     *      THEN
            CALL RWRMSF (NC, NT, NA, NB, NI, NCT, CHFLGS, B, AV, S,
     *         NVM, VM, COUNT)
            J1 = (NT+1)/2
            J2 = J1
            IF (FIRST) J1 = 1
            FIRST = .FALSE.
            DO 140 JA1 = 1,NA
               DO 130 JA2 = JA1,NA
                  DO 110 JI = 1,NI
                     DO 105 JP = 1,NSTOK
                        IF (AV(JP,JI,JA1,JA2).NE.0.0) GO TO 115
 105                    CONTINUE
 110                 CONTINUE
                  GO TO 130
C                                       write data
 115              RECORD(DATP(2,1)) = 256 * JA1 + JA2
                  JP = 4 * NI
                  CALL RCOPY (JP, AV(1,1,JA1,JA2), RECR(DATP(4,1)))
                  DO 120 J = J1,J2
                     NRBL(JA1,JA2) = NRBL(JA1,JA2) + 1
                     NTIMES = MAX (NTIMES, NRBL(JA1,JA2))
                     RECORD(DATP(1,1)) = VISN(1,TNUM(J))
                     RECORD(DATP(1,1)+1) = VISN(2,TNUM(J))
                     RECR(DATP(3,1)) = TIMES(TNUM(J))
                     NREC = NREC + 1
                     CALL TABIO ('WRIT', 0, NREC, RECORD, TABUFF, IRET)
                     IF (IRET.NE.0) THEN
                        WRITE (MSGTXT,1000) IRET, 'WRITING TEMP TABLE'
                        GO TO 990
                        END IF
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
            CT = TNUM(1)
            DO 150 JT = 1,NT-1
               TNUM(JT) = TNUM(JT+1)
 150           CONTINUE
            TNUM(NT) = CT
            TIMES(CT) = TIME
            VISN(1,CT) = NUMVIS
            VISN(2,CT) = NUMVIS
            CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B)
            GOTONE = .TRUE.
            TB = TIME
C                                       done with this scan
         ELSE
            IF (GOTONE) THEN
               CALL RWRMSF (NC, NT, NA, NB, NI, NCT, CHFLGS, B, AV, S,
     *            NVM, VM, COUNT)
               J1 = (NT+1)/2
               J2 = NCT
               IF (FIRST) J1 = 1
               DO 240 JA1 = 1,NA
                  DO 230 JA2 = JA1,NA
                     DO 210 JI = 1,NI
                        DO 205 JP = 1,NSTOK
                           IF (AV(JP,JI,JA1,JA2).NE.0.0) GO TO 215
 205                       CONTINUE
 210                    CONTINUE
                     GO TO 230
C                                       write data
 215                 RECORD(DATP(2,1)) = 256 * JA1 + JA2
                     JP = 4 * NI
                     CALL RCOPY (JP, AV(1,1,JA1,JA2), RECR(DATP(4,1)))
                     DO 220 J = J1,J2
                        NRBL(JA1,JA2) = NRBL(JA1,JA2) + 1
                        NTIMES = MAX (NTIMES, NRBL(JA1,JA2))
                        RECORD(DATP(1,1)) = VISN(1,TNUM(J))
                        RECORD(DATP(1,1)+1) = VISN(2,TNUM(J))
                        RECR(DATP(3,1)) = TIMES(TNUM(J))
                        NREC = NREC + 1
                        CALL TABIO ('WRIT', 0, NREC, RECORD, TABUFF,
     *                     IRET)
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,1000) IRET,
     *                        'WRITING TEMP TABLE'
                           GO TO 990
                           END IF
 220                    CONTINUE
 230                 CONTINUE
 240              CONTINUE
               END IF
            IF (.NOT.END) THEN
               CALL RFILL (NW, 0.0, B)
               FIRST = .TRUE.
               CT = 1
               NCT = 1
               TNUM(1) = 1
               TIMES(1) = TIME
               TIMES(CT) = TIME
               VISN(1,CT) = NUMVIS
               VISN(2,CT) = NUMVIS
               CALL RWPUTB (NC, NT, NB, NI, CT, RPARM, VIS, B)
               TB = TIME
               LSOU = ISOU
               GOTONE = .TRUE.
               END IF
            END IF
         IF (.NOT.END) GO TO 100
         END IF
C                                       all in finally!
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING TEMP TABLE'
         GO TO 990
         END IF
C                                       close uv data set
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
C                                       find averages and rmses
      SA(1) = 0.0
      SA(2) = 0.0
      SSA = 0.0
      DO 280 JA1 = 1,NA
         DO 270 JA2 = JA1,NA
            DO 260 JI = 1,NI
               DO 250 JP = 1,NSTOK
                  IF (S(3,JA1,JA2,JI,JP).GT.0.5) THEN
                     S(1,JA1,JA2,JI,JP) = S(1,JA1,JA2,JI,JP) /
     *                  S(3,JA1,JA2,JI,JP)
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) /
     *                  S(3,JA1,JA2,JI,JP)
                     S(2,JA1,JA2,JI,JP) = S(2,JA1,JA2,JI,JP) -
     *                  S(1,JA1,JA2,JI,JP)**2
                     IF (S(2,JA1,JA2,JI,JP).GT.0.0) THEN
                        W = 1.0 / S(2,JA1,JA2,JI,JP)
                        SSA = SSA + W
                        SA(1) = SA(1) + W * S(1,JA1,JA2,JI,JP)
                        SA(2) = SA(2) + W * S(1,JA1,JA2,JI,JP) *
     *                     S(1,JA1,JA2,JI,JP)
                        END IF
                     S(2,JA1,JA2,JI,JP) = SQRT
     *                  (MAX (0.0, S(2,JA1,JA2,JI,JP)))
                     END IF
 250              CONTINUE
 260           CONTINUE
 270        CONTINUE
 280     CONTINUE
      IF (SSA.GT.0.0) THEN
         SA(1) = SA(1) / SSA
         SA(2) = SA(2) / SSA - SA(1) * SA(1)
         SA(2) = SQRT (MAX (0.0, SA(2)))
         END IF
      IF (SA(2).LE.0.0) SA(2) = 1.E6
C                                       sort WT table
      MSGTXT = 'Sort, smooth, re-sort the WT table'
      CALL MSGWRT (2)
      KEY(1,1) = 2
      KEY(2,1) = 0
      KEY(1,2) = 3
      KEY(2,2) = 0
      CALL TABSRT (DISKO, NEWCNO, 'WT', VER, VER, KEY, KEYSUB, FKEY,
     *   TABUFF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SORTING TEMP TABLE'
         GO TO 990
         END IF
C                                       re-open WT table
      OPCODE = 'WRIT'
      CALL TABINI (OPCODE, 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPENING TEMP TABLE'
         GO TO 990
         END IF
C                                       time smooth
      NW1 = (NTIMES * 4 * NI + 6124) / 1024 + 1
      NW2 = (NTIMES * 3 + 6124) / 1024 + 1
      CALL ZMEMRY ('GET ', 'RWAYFB', NW1, WTSBUF, PWTS, IRET)
      IF (IRET.EQ.0) CALL ZMEMRY ('GET ', 'RWAYFB', NW2, WRKBUF,
     *   PWRK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET,
     *      'GETTING DYNAMIC MEMORY FOR SMOOTHING'
         GO TO 990
         END IF
      CALL RWAYSC (DATP, NA, NI, SA, S, TABUFF, WTSBUF(1+PWTS),
     *   WRKBUF(1+PWRK), COUNT(4), IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SMOOTHING THE TEMP TABLE'
         GO TO 990
         END IF
      CALL ZMEMRY ('FREE', 'RWAYFB', NW1, WTSBUF, PWTS, IRET)
      CALL ZMEMRY ('FREE', 'RWAYFB', NW2, WRKBUF, PWRK, IRET)
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CLOSING TEMP TABLE'
         GO TO 990
         END IF
C                                       sort WT table
      KEY(1,1) = 1
      KEY(2,1) = 0
      KEY(1,2) = 3
      KEY(2,2) = 0
      CALL TABSRT (DISKO, NEWCNO, 'WT', VER, VER, KEY, KEYSUB, FKEY,
     *   TABUFF, CATBLK, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-SORTING TEMP TABLE'
         GO TO 990
         END IF
C                                       reopen to do output
      NREC = 0
C                                       re-open WT table
      OPCODE = 'READ'
      CALL TABINI (OPCODE, 'WT', DISKO, NEWCNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, TABUFF, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-RE-OPENING TEMP TABLE'
         GO TO 990
         END IF
      MAXREC = TABUFF(5)
C                                       defend cat header from UVGET
      CALL COPY (256, CATBLK, CATMP)
C                                       Open and init for read
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-OPEN/INIT INPUT VIS FILE'
         GO TO 990
         END IF
      CALL COPY (256, CATMP, CATBLK)
      CALL UVPGET (IRET)
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'INITING OUTPUT VIS FILE'
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
      NUMVIS = 0
      XCOUNT = 0
      FVIS = 0
      NW = 4 * NI
      VISN(1,1) = 0
      VISN(2,1) = 0
      MSGTXT = 'Apply the WT table'
      CALL MSGWRT (2)
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
 300  CALL UVGET ('READ', RPARM, VIS, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET, 'RE-READING INPUT VIS FILE'
         GO TO 990
C                                       Loop over buffer
      ELSE IF (IRET.EQ.0) THEN
         NUMVIS = NUMVIS + 1
         IF (MOD(NUMVIS,50000).EQ.0) THEN
            WRITE (MSGTXT,1100) NUMVIS
            CALL MSGWRT (2)
            END IF
C                                       get some table data
         IF (NUMVIS.GT.VISN(2,1)) THEN
            VISN(1,1) = 100000000
            VISN(2,1) = 0
 310        NREC = NREC + 1
            CALL TABIO ('READ', 0, NREC, RECORD, TABUFF, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'READING TEMP TABLE'
               GO TO 990
               END IF
            J1 = RECORD(DATP(1,1))
            J2 = RECORD(DATP(1,1)+1)
            IF ((J1.LE.NUMVIS) .AND. (J2.GE.NUMVIS)) THEN
               VISN(1,1) = MIN (VISN(1,1), J1)
               VISN(2,1) = MAX (VISN(2,1), J2)
               JA2 = RECORD(DATP(2,1))
               JA1 = JA2 / 256
               JA2 = JA2 - 256*JA1
               NW = 4 * NI
               CALL RCOPY (NW, RECR(DATP(4,1)), AV(1,1,JA1,JA2))
               IF (NREC.LT.TABUFF(5)) GO TO 310
            ELSE
               NREC = NREC - 1
               END IF
            END IF
         RPARM(1+ILOCU) = RPARM(1+ILOCU) * UVSCAL
         RPARM(1+ILOCV) = RPARM(1+ILOCV) * UVSCAL
         RPARM(1+ILOCW) = RPARM(1+ILOCW) * UVSCAL
         IF (ILOCB.GE.0) THEN
            BASEN = RPARM(1+ILOCB)
            JA1 = BASEN / 256. + 0.1
            JA2 = BASEN - JA1*256. + 0.1
         ELSE
            JA1 = RPARM(1+ILOCA1) + 0.1
            JA2 = RPARM(1+ILOCA2) + 0.1
            END IF
C                                       apply result
         GOTONE = .FALSE.
         DO 340 JI = 1,NI
            DO 330 JP = 1,NSTOK
               KP = (JP + 1) / 2
               IF (MOD(JP,2).EQ.0) THEN
                  KA1 = JA2
                  KA2 = JA1
               ELSE
                  KA1 = JA1
                  KA2 = JA2
                  END IF
               INDEXI = (JI-1) * INCIFI + (JP-1) * INCSI + 1
               INDEXO = (JI-1) * INCIFO + (JP-1) * INCSO + 1
               WT = AV(JP,JI,JA1,JA2)
               IF (WT.GT.0.0) THEN
                  WT = 1.0 / (WT * WT)
                  IF (NSUMS(KA1,KA2,JI,KP).LE.0) THEN
                     MNWTS(KA1,KA2,JI,KP) = WT
                     MXWTS(KA1,KA2,JI,KP) = WT
                     SUMWTS(KA1,KA2,JI,KP) = WT
                     SUMSQW(KA1,KA2,JI,KP) = WT * WT
                     NSUMS(KA1,KA2,JI,KP) = 1
                  ELSE
                     MNWTS(KA1,KA2,JI,KP) =
     *                  MIN (MNWTS(KA1,KA2,JI,KP),WT)
                     MXWTS(KA1,KA2,JI,KP) =
     *                  MAX (MXWTS(KA1,KA2,JI,KP),WT)
                     SUMWTS(KA1,KA2,JI,KP) = SUMWTS(KA1,KA2,JI,KP) +
     *                  WT
                     SUMSQW(KA1,KA2,JI,KP) = SUMSQW(KA1,KA2,JI,KP) +
     *                  WT * WT
                     NSUMS(KA1,KA2,JI,KP) = NSUMS(KA1,KA2,JI,KP) + 1
                     END IF
                  END IF
               DO 320 JF = 1,NCHAN
                  RESULT(1,INDEXO) = VIS(1,INDEXI)
                  RESULT(2,INDEXO) = VIS(2,INDEXI)
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     IF (NOWAY.GE.2) THEN
                        RESULT(3,INDEXO) = WT
                     ELSE IF (NOWAY.EQ.1) THEN
                        RESULT(3,INDEXO) = WT * RPARM(1+ILOCIT)
                     ELSE
                        RESULT(3,INDEXO) = WT * VIS(3,INDEXI)
                        END IF
                  ELSE
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
                     END IF
                  IF (RESULT(3,INDEXO).GT.0.0) GOTONE = .TRUE.
                  INDEXI = INDEXI + INCFI
                  INDEXO = INDEXO + INCFO
 320              CONTINUE
 330           CONTINUE
 340        CONTINUE
C                                       don't write fully flagged
         IF (GOTONE) THEN
            XCOUNT = XCOUNT + 1
            CALL RCOPY (NRPRMI, RPARM, BUFF2(IPTRO))
            CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
C                                       update NX table
            CALL RNXUPD (RPARM, RNXRET)
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            END IF
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITING OUTPUT VIS FILE'
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
C                                       Read next buffer.
         GO TO 300
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'FLUSHING OUTPUT VIS FILE'
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
C                                       Close files
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
C                                       close WX table
      CALL TABIO ('CLOS', 0, NREC, RECORD, TABUFF, IRET)
C                                       close NX table
      IRET = 0
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      XF = 100.0 * COUNT(2)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1240) XF
      CALL MSGWRT (4)
      XF = 100.0 * COUNT(3)
      IF (COUNT(1).GT.0) XF = XF / COUNT(1)
      WRITE (MSGTXT,1241) XF
      CALL MSGWRT (4)
      WRITE (MSGTXT,1242) COUNT(4)
      IF (COUNT(4).GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT,1243) COUNT(5)
      IF (COUNT(5).GT.0) CALL MSGWRT (4)
      WRITE (MSGTXT,1244) COUNT(6)
      IF (COUNT(6).GT.0) CALL MSGWRT (4)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RWAYFB: ERROR',I3,' ON ',A)
 1100 FORMAT ('RWAYFB: on visibility number',I10)
 1240 FORMAT (F7.4,' % of spectra had Real/Imag rms > 1.5')
 1241 FORMAT (F7.4,' % of spectra had Imag/Real rms > 1.5')
 1242 FORMAT (I10,' spectra flagged due to smoothing and clipping')
 1243 FORMAT (I10,' spectra previously flagged')
 1244 FORMAT (I10,' spectra too few points')
      END
      SUBROUTINE FLAGIT (OPCODE, LUN, DISK, CNO, VERI, VER, LFGRNO,
     *   FGKOLS, FGNUMV, SUBA, FQID, ANT1, ANT2, BTIME, ETIME, IFNUM,
     *   POLNUM, REASON, CATUV, BUFF, IRET)
C-----------------------------------------------------------------------
C   Updates the Flag (FG) table. Adapted from FLAGUP
C   One entry is made indicating a visibility to be rejected.
C   The FLAG table will be opened on the first call but a final call
C   with OPCODE='CLOS' is required to close the file.
C   Inputs:
C      OPCODE   C*4      Operation desired, 'CLOS'=>close file
C                        Anything else = 'FLAG'
C      DISK     I        Disk to use.
C      CNO      I        Catalog slot number
C      VERI     I        Input version number
C      VER      I        FG file version
C      LUN      I        Logical unit number to use
C      NID      I        Number of elements in ID
C      SUBA     I        Subarray number.
C      FQID     I        Freqid number
C      ANT1     I        First antenna number in baseline
C      ANT2     I        Second antenna number in baseline
C      BTIME    R        Start time of data to be flagged (Days)
C      ETIME    R        End time of data to be flagged (Days)
C      IFNUM    I        IF number to flag
C      POLNUM   I        Polarization number to flag 1-4
C      REASON   C*24     Reason for flagging blank => ignore for unflag.
C   Input/Output:
C      CATUV    I(256)   Header for disk file to get FG table
C      BUFF     I(512)   I/O buffer and related storage, also defines
C                        file if open.
C      LFGRNO   I        Next scan number, start of the file if 'READ',
C                        the last+1 if WRITE
C      FGKOLS   I(*)     The column pointer array in order, SOURCE,
C                        SUBARRAY, ANTS, TIMERANG, IFS, CHANS, PFLAGS,
C                        REASON
C      FGNUMV   I(*)     Element count in each column.
C   In/Out:
C      IRET     I        Error code: > 0 on input -> return
C                        0=>OK else TABIO error.
C                        Note: -1 => read, but record deselected.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, REASON*24
      INTEGER   LUN, DISK, CNO, VERI, VER, LFGRNO, FGKOLS(*), FGNUMV(*),
     *   SUBA, FQID, ANT1, ANT2, IFNUM, POLNUM, CATUV(256),
     *   BUFF(*), IRET
      REAL      BTIME, ETIME
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER TREAS*24, CTEMP*12
      INTEGER   IDT, SUBT, ANTS(2), IFS(2), CHANS(2), IDUM, FIND, I,
     *   BUFF2(512), LUN2, IFGKOL(MAXFGC), IFGNUM(MAXFGC), NROW, IFQ,
     *   IFGRNO, ID
      LOGICAL   PFLAGS(4), TFLAGS(4), FIRST
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
C                                       See if table open - check FTAB
      IF (OPCODE.NE.'CLOS') THEN
         IF (IRET.GT.0) GO TO 999
         FIND = BUFF(82)
C                                       Open file
         IF ((FIND.LT.0) .OR. (FIND.GT.10000) .OR. (LUN.NE.FTAB(FIND)))
     *      THEN
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'CLRD', BUFF, IRET)
            CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV',
     *         IDUM, 'WRIT', BUFF, IRET)
C
            CALL FLGINI ('WRIT', BUFF, DISK, CNO, VER, CATUV, LUN,
     *         LFGRNO, FGKOLS, FGNUMV, IRET)
C                                       Report on the need for flagging
            WRITE (MSGTXT,1000) VER
            IF (.NOT.FIRST) WRITE (MSGTXT,1001) VER
            CALL MSGWRT (2)
            IF (IRET.NE.0) GO TO 999
C                                       Copy the old file
            IF ((FIRST) .AND. (VERI.GT.0)) THEN
               LUN2 = LUN + 1
               CALL FLGINI ('READ', BUFF2, DISK, CNO, VERI, CATUV, LUN2,
     *            IFGRNO, IFGKOL, IFGNUM, IRET)
               IF (IRET.NE.0) GO TO 999
               NROW = BUFF2(5)
               WRITE (MSGTXT,1002) NROW, VERI, VER
               CALL MSGWRT (2)
               DO 20 I = 1,NROW
                  CALL TABFLG ('READ', BUFF2, IFGRNO, IFGKOL, IFGNUM,
     *               IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *               TREAS, IRET)
                  IF (IRET.GT.0) GO TO 999
                  IF (IRET.EQ.0) THEN
                     CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV,
     *                  IDT, SUBT, IFQ, ANTS, TIMER, IFS, CHANS, TFLAGS,
     *                  TREAS, IRET)
                     IF (IRET.NE.0) GO TO 999
                     END IF
 20               CONTINUE
               CALL TABIO ('CLOS', 0, IFGRNO, BUFF2, BUFF2, I)
               END IF
            FIRST = .FALSE.
C                                       Mark as unsorted
            BUFF(43) = 0
            BUFF(44) = 0
            END IF
C                                       Set up for flagging
         ANTS(1) = ANT1
         ANTS(2) = ANT2
         TIMER(1) = BTIME
         TIMER(2) = ETIME
         IFS(1) = IFNUM
         IFS(2) = IFNUM
         CHANS(1) = 1
         CHANS(2) = 0
         PFLAGS(1) = .FALSE.
         PFLAGS(2) = .FALSE.
         PFLAGS(3) = .FALSE.
         PFLAGS(4) = .FALSE.
         PFLAGS(POLNUM) = .TRUE.
         ID = 0
C                                       Flag table entry.
         CALL TABFLG ('WRIT', BUFF, LFGRNO, FGKOLS, FGNUMV, ID, SUBA,
     *      FQID, ANTS, TIMER, IFS, CHANS, PFLAGS, REASON, IRET)
C                                       Close
      ELSE IF (.NOT.FIRST) THEN
         CALL TABFLG ('CLOS', BUFF, LFGRNO, FGKOLS, FGNUMV, IDT, SUBT,
     *      FQID, ANTS, TIMER, IFS, CHANS, TFLAGS, TREAS, IRET)
C                                       Clear write status
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'CLWR', BUFF, IRET)
C                                       Reset status to read
         CALL CATDIR ('CSTA', DISK, CNO, CTEMP, CTEMP, IDUM, 'UV', IDUM,
     *      'READ', BUFF, IRET)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Found some bad data, will write flags to table FG', I4)
 1001 FORMAT ('Found some bad data, will add   flags to table FG', I4)
 1002 FORMAT ('Copy',I8,' rows from FG vers',I3,' to',I3)
      END
      SUBROUTINE REPANT (IRET)
C-----------------------------------------------------------------------
C   Reports antenna-based statistics
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'REWAY.INC'
      INTEGER   IANT, IS, IIF, NSTAT(MAXIF,4), TXLUN, TXIND, IERR, I,
     *   M
      LOGICAL   DOTX, DOIT
      REAL      MNSTAT(4,MAXIF,4), MXSTAT(4,MAXIF,4), VSTAT(4,MAXIF,4),
     *   RSTAT(4,MAXIF,4), X
      CHARACTER MARK(4)*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IS = 4 * MAXIF
      CALL FILL (IS, 0, NSTAT)
      DO 30 IANT = 1,NANT
         DO 20 IIF = 1,NIF
            DO 10 IS = 1,NSTOK
               IF (NSUMS(IANT,IS,IIF,1).GT.0) THEN
                  SUMWTS(IANT,IS,IIF,1) = SUMWTS(IANT,IS,IIF,1) /
     *               NSUMS(IANT,IS,IIF,1)
                  SUMSQW(IANT,IS,IIF,1) = SUMSQW(IANT,IS,IIF,1) /
     *               NSUMS(IANT,IS,IIF,1)
                  SUMSQW(IANT,IS,IIF,1) = SUMSQW(IANT,IS,IIF,1) -
     *               SUMWTS(IANT,IS,IIF,1)**2
                  SUMSQW(IANT,IS,IIF,1) = SQRT (MAX (0.0,
     *               SUMSQW(IANT,IS,IIF,1)))
                  IF (NSTAT(IIF,IS).LE.0) THEN
                     NSTAT(IIF,IS) = 1
                     MNSTAT(1,IIF,IS) = MNWTS(IANT,IS,IIF,1)
                     MNSTAT(2,IIF,IS) = MNWTS(IANT,IS,IIF,1)
                     MNSTAT(3,IIF,IS) = MNWTS(IANT,IS,IIF,1)
                     MNSTAT(4,IIF,IS) = MNWTS(IANT,IS,IIF,1)**2
                     MXSTAT(1,IIF,IS) = MXWTS(IANT,IS,IIF,1)
                     MXSTAT(2,IIF,IS) = MXWTS(IANT,IS,IIF,1)
                     MXSTAT(3,IIF,IS) = MXWTS(IANT,IS,IIF,1)
                     MXSTAT(4,IIF,IS) = MXWTS(IANT,IS,IIF,1)**2
                     VSTAT(1,IIF,IS) = SUMWTS(IANT,IS,IIF,1)
                     VSTAT(2,IIF,IS) = SUMWTS(IANT,IS,IIF,1)
                     VSTAT(3,IIF,IS) = SUMWTS(IANT,IS,IIF,1)
                     VSTAT(4,IIF,IS) = SUMWTS(IANT,IS,IIF,1)**2
                     RSTAT(1,IIF,IS) = SUMSQW(IANT,IS,IIF,1)
                     RSTAT(2,IIF,IS) = SUMSQW(IANT,IS,IIF,1)
                     RSTAT(3,IIF,IS) = SUMSQW(IANT,IS,IIF,1)
                     RSTAT(4,IIF,IS) = SUMSQW(IANT,IS,IIF,1)**2
                  ELSE
                     NSTAT(IIF,IS) = NSTAT(IIF,IS) + 1
                     MNSTAT(1,IIF,IS) = MIN (MNWTS(IANT,IS,IIF,1),
     *                  MNSTAT(1,IIF,IS))
                     MNSTAT(2,IIF,IS) = MAX (MNWTS(IANT,IS,IIF,1),
     *                  MNSTAT(2,IIF,IS))
                     MNSTAT(3,IIF,IS) = MNWTS(IANT,IS,IIF,1) +
     *                  MNSTAT(3,IIF,IS)
                     MNSTAT(4,IIF,IS) = MNWTS(IANT,IS,IIF,1)**2 +
     *                  MNSTAT(4,IIF,IS)
                     MXSTAT(1,IIF,IS) = MIN (MXWTS(IANT,IS,IIF,1),
     *                  MXSTAT(1,IIF,IS))
                     MXSTAT(2,IIF,IS) = MAX (MXWTS(IANT,IS,IIF,1),
     *                  MXSTAT(2,IIF,IS))
                     MXSTAT(3,IIF,IS) = MXWTS(IANT,IS,IIF,1) +
     *                  MXSTAT(3,IIF,IS)
                     MXSTAT(4,IIF,IS) = MXWTS(IANT,IS,IIF,1)**2 +
     *                  MXSTAT(4,IIF,IS)
                     VSTAT(1,IIF,IS) = MIN (SUMWTS(IANT,IS,IIF,1),
     *                  VSTAT(1,IIF,IS))
                     VSTAT(2,IIF,IS) = MAX (SUMWTS(IANT,IS,IIF,1),
     *                  VSTAT(2,IIF,IS))
                     VSTAT(3,IIF,IS) = SUMWTS(IANT,IS,IIF,1) +
     *                  VSTAT(3,IIF,IS)
                     VSTAT(4,IIF,IS) = SUMWTS(IANT,IS,IIF,1)**2 +
     *                  VSTAT(4,IIF,IS)
                     RSTAT(1,IIF,IS) = MIN (SUMSQW(IANT,IS,IIF,1),
     *                  RSTAT(1,IIF,IS))
                     RSTAT(2,IIF,IS) = MAX (SUMSQW(IANT,IS,IIF,1),
     *                  RSTAT(2,IIF,IS))
                     RSTAT(3,IIF,IS) = SUMSQW(IANT,IS,IIF,1) +
     *                  RSTAT(3,IIF,IS)
                     RSTAT(4,IIF,IS) = SUMSQW(IANT,IS,IIF,1)**2 +
     *                  RSTAT(4,IIF,IS)
                     END IF
                  END IF
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
      DO 50 IIF = 1,NIF
         DO 40 IS = 1,NSTOK
            IF (NSTAT(IIF,IS).GT.0) THEN
               MNSTAT(3,IIF,IS) = MNSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               MNSTAT(4,IIF,IS) = MNSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               MNSTAT(4,IIF,IS) = MNSTAT(4,IIF,IS) - MNSTAT(3,IIF,IS)**2
               MNSTAT(4,IIF,IS) = SQRT (MAX (0.0, MNSTAT(4,IIF,IS)))
               MXSTAT(3,IIF,IS) = MXSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               MXSTAT(4,IIF,IS) = MXSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               MXSTAT(4,IIF,IS) = MXSTAT(4,IIF,IS) - MXSTAT(3,IIF,IS)**2
               MXSTAT(4,IIF,IS) = SQRT (MAX (0.0, MXSTAT(4,IIF,IS)))
               VSTAT(3,IIF,IS) = VSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               VSTAT(4,IIF,IS) = VSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               VSTAT(4,IIF,IS) = VSTAT(4,IIF,IS) - VSTAT(3,IIF,IS)**2
               VSTAT(4,IIF,IS) = SQRT (MAX (0.0, VSTAT(4,IIF,IS)))
               RSTAT(3,IIF,IS) = RSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               RSTAT(4,IIF,IS) = RSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               RSTAT(4,IIF,IS) = RSTAT(4,IIF,IS) - RSTAT(3,IIF,IS)**2
               RSTAT(4,IIF,IS) = SQRT (MAX (0.0, RSTAT(4,IIF,IS)))
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Now tell the user some stuff
      DOTX = OUTEXT.NE.' '
      IF (DOTX) THEN
         TXLUN = 3
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTEXT, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT TEXT FILE'
            CALL MSGWRT (7)
            DOTX = .FALSE.
            TXIND = 0
            END IF
      ELSE
         TXIND = 0
         END IF
C                                       summary stats
      MSGTXT = 'Minimum antenna weight averaged over all antennas'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 110 IS = 1,NSTOK
         DO 100 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (MNSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 100        CONTINUE
 110     CONTINUE
      MSGTXT = 'Maximum antenna weight averaged over all antennas'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 130 IS = 1,NSTOK
         DO 120 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (MXSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 120        CONTINUE
 130     CONTINUE
      MSGTXT = 'Mean antenna weight averaged over all antennas'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 150 IS = 1,NSTOK
         DO 140 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (VSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 140        CONTINUE
 150     CONTINUE
      MSGTXT = 'RMS of antenna weight averaged over all antennas'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 170 IS = 1,NSTOK
         DO 160 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (RSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 160        CONTINUE
 170     CONTINUE
C                                       outliers
      IF (BPARM(3).LE.1.0) BPARM(3) = 5.0
      MSGTXT = ' '
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      MSGTXT = 'Outliers in antenna weight'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1199)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 220 IS = 1,NSTOK
         DO 215 IIF = 1,NIF
            DO 210 IANT = 1,NANT
               DOIT = .FALSE.
               IF (NSUMS(IANT,IS,IIF,1).GT.0) THEN
                  X = ABS (MNWTS(IANT,IS,IIF,1)-MNSTAT(3,IIF,IS))
                  IF (X.GT.BPARM(3)*MNSTAT(4,IIF,IS)) THEN
                     DOIT = .TRUE.
                     MARK(1) = '*'
                  ELSE
                     MARK(1) = ' '
                     END IF
                  X = ABS (MXWTS(IANT,IS,IIF,1)-MXSTAT(3,IIF,IS))
                  IF (X.GT.BPARM(3)*MXSTAT(4,IIF,IS)) THEN
                     DOIT = .TRUE.
                     MARK(2) = '*'
                  ELSE
                     MARK(2) = ' '
                     END IF
                  X = ABS (SUMWTS(IANT,IS,IIF,1)-VSTAT(3,IIF,IS))
                  IF (X.GT.BPARM(3)*VSTAT(4,IIF,IS)) THEN
                     DOIT = .TRUE.
                     MARK(3) = '*'
                  ELSE
                     MARK(3) = ' '
                     END IF
                  X = ABS (SUMSQW(IANT,IS,IIF,1)-RSTAT(3,IIF,IS))
                  IF (X.GT.BPARM(3)*RSTAT(4,IIF,IS)) THEN
                     DOIT = .TRUE.
                     MARK(4) = '*'
                  ELSE
                     MARK(4) = ' '
                     END IF
                  IF (DOIT) THEN
                     WRITE (MSGTXT,1200) IANT, IIF, IS,
     *                  MNWTS(IANT,IS,IIF,1), MARK(1),
     *                  MXWTS(IANT,IS,IIF,1), MARK(2),
     *                  SUMWTS(IANT,IS,IIF,1), MARK(3),
     *                  SUMSQW(IANT,IS,IIF,1), MARK(4)
                     CALL DOMSG (5, DOTX, TXLUN, TXIND)
                     END IF
                  END IF
 210           CONTINUE
 215        CONTINUE
 220     CONTINUE
C                                       detailed stats
C                                       we can tell all in message
      IF ((DOTX) .OR. (BPARM(2).GE.2.0)) THEN
         M = -1
         IF (BPARM(2).GE.2.0) M = 5
         MSGTXT = ' '
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         MSGTXT = 'All antenna weights'
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         WRITE (MSGTXT,1199)
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         DO 320 IS = 1,NSTOK
            DO 315 IIF = 1,NIF
               DO 310 IANT = 1,NANT
                  IF (NSUMS(IANT,IS,IIF,1).GT.0) THEN
                     WRITE (MSGTXT,1300) IANT, IIF, IS,
     *                  MNWTS(IANT,IS,IIF,1), MXWTS(IANT,IS,IIF,1),
     *                  SUMWTS(IANT,IS,IIF,1), SUMSQW(IANT,IS,IIF,1)
                     CALL DOMSG (M, DOTX, TXLUN, TXIND)
                     END IF
 310              CONTINUE
 315           CONTINUE
 320        CONTINUE
         END IF
C                                       Close
      IF (TXIND.GT.0) CALL ZTXCLS (TXLUN, TXIND, IERR)
      IRET = 0
      IF ((OUTEXT.NE.' ') .AND. (.NOT.DOTX)) IRET = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REPANT ERROR',I4,' ON ',A)
 1099 FORMAT ('  IF Pol      Minimum',6X,'Maximum',6X,'Mean',9X,'RMS')
 1100 FORMAT (2I4,4F13.4)
 1199 FORMAT (' Ant  IF Pol      Minimum',6X,'Maximum',6X,'Mean',9X,
     *   'RMS')
 1200 FORMAT (3I4,4(F12.3,A1))
 1300 FORMAT (3I4,4F13.4)
      END
      SUBROUTINE DOMSG (MSGLEV, DOTX, TXLUN, TXIND)
C-----------------------------------------------------------------------
C   writes message to message file and/or text file
C   Inputs:
C      MSGLEV   I   Message level (< 0 -> no MSGWRT)
C      TXLUN    I   Text file lun
C      TXIND    I   Text file FTAB pointer
C   IN (common)
C      MSGTXT   C*(*)   Message text
C   In/Out
C      DOTX     L   Do text file - set false on write error
C-----------------------------------------------------------------------
      INTEGER   MSGLEV, TXLUN, TXIND
      LOGICAL   DOTX
C
      INTEGER   J, JTRIM, IERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IF (MSGLEV.GE.0) CALL MSGWRT (MSGLEV)
      IF (DOTX) THEN
         J = JTRIM (MSGTXT)
         J = MAX (1, J)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, MSGTXT(:J), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITING OUTPUT TEXT FILE'
            CALL MSGWRT (8)
            DOTX = .FALSE.
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DOMSG ERROR',I4,' ON ',A)
      END
      SUBROUTINE REPBL (IRET)
C-----------------------------------------------------------------------
C   Reports antenna-based statistics
C   Output:
C      IRET
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'REWAY.INC'
      INTEGER   IA1, IA2, JA1, JA2, IS, JS, IIF, NSTAT(MAXIF,4), TXLUN,
     *   TXIND, IERR, I, M
      LOGICAL   DOTX, DOIT
      REAL      MNSTAT(4,MAXIF,4), MXSTAT(4,MAXIF,4), VSTAT(4,MAXIF,4),
     *   RSTAT(4,MAXIF,4), X
      CHARACTER MARK(4)*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IS = 4 * MAXIF
      CALL FILL (IS, 0, NSTAT)
      DO 30 JS = 1,NSTOK
         IS = (JS + 1) / 2
         DO 20 IIF = 1,NIF
            DO 15 IA1 = 1,NANT-1
               DO 10 IA2 = IA1+1,NANT
                  IF (MOD(JS,2).EQ.0) THEN
                     JA1 = IA2
                     JA2 = IA1
                  ELSE
                     JA1 = IA1
                     JA2 = IA2
                     END IF
                  IF (NSUMS(JA1,JA2,IIF,IS).GT.0) THEN
                     SUMWTS(JA1,JA2,IIF,IS) = SUMWTS(JA1,JA2,IIF,IS) /
     *                  NSUMS(JA1,JA2,IIF,IS)
                     SUMSQW(JA1,JA2,IIF,IS) = SUMSQW(JA1,JA2,IIF,IS) /
     *                  NSUMS(JA1,JA2,IIF,IS)
                     SUMSQW(JA1,JA2,IIF,IS) = SUMSQW(JA1,JA2,IIF,IS) -
     *                  SUMWTS(JA1,JA2,IIF,IS)**2
                     SUMSQW(JA1,JA2,IIF,IS) = SQRT (MAX (0.0,
     *                  SUMSQW(JA1,JA2,IIF,IS)))
                     IF (NSTAT(IIF,JS).LE.0) THEN
                        NSTAT(IIF,JS) = 1
                        MNSTAT(1,IIF,JS) = MNWTS(JA1,JA2,IIF,IS)
                        MNSTAT(2,IIF,JS) = MNWTS(JA1,JA2,IIF,IS)
                        MNSTAT(3,IIF,JS) = MNWTS(JA1,JA2,IIF,IS)
                        MNSTAT(4,IIF,JS) = MNWTS(JA1,JA2,IIF,IS)**2
                        MXSTAT(1,IIF,JS) = MXWTS(JA1,JA2,IIF,IS)
                        MXSTAT(2,IIF,JS) = MXWTS(JA1,JA2,IIF,IS)
                        MXSTAT(3,IIF,JS) = MXWTS(JA1,JA2,IIF,IS)
                        MXSTAT(4,IIF,JS) = MXWTS(JA1,JA2,IIF,IS)**2
                        VSTAT(1,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS)
                        VSTAT(2,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS)
                        VSTAT(3,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS)
                        VSTAT(4,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS)**2
                        RSTAT(1,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS)
                        RSTAT(2,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS)
                        RSTAT(3,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS)
                        RSTAT(4,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS)**2
                     ELSE
                        NSTAT(IIF,JS) = NSTAT(IIF,IS) + 1
                        MNSTAT(1,IIF,JS) = MIN (MNWTS(JA1,JA2,IIF,IS),
     *                     MNSTAT(1,IIF,JS))
                        MNSTAT(2,IIF,JS) = MAX (MNWTS(JA1,JA2,IIF,IS),
     *                     MNSTAT(2,IIF,JS))
                        MNSTAT(3,IIF,JS) = MNWTS(JA1,JA2,IIF,IS) +
     *                     MNSTAT(3,IIF,JS)
                        MNSTAT(4,IIF,JS) = MNWTS(JA1,JA2,IIF,IS)**2 +
     *                     MNSTAT(4,IIF,JS)
                        MXSTAT(1,IIF,JS) = MIN (MXWTS(JA1,JA2,IIF,IS),
     *                     MXSTAT(1,IIF,JS))
                        MXSTAT(2,IIF,JS) = MAX (MXWTS(JA1,JA2,IIF,IS),
     *                     MXSTAT(2,IIF,JS))
                        MXSTAT(3,IIF,JS) = MXWTS(JA1,JA2,IIF,IS) +
     *                     MXSTAT(3,IIF,JS)
                        MXSTAT(4,IIF,JS) = MXWTS(JA1,JA2,IIF,IS)**2 +
     *                     MXSTAT(4,IIF,JS)
                        VSTAT(1,IIF,JS) = MIN (SUMWTS(JA1,JA2,IIF,IS),
     *                     VSTAT(1,IIF,JS))
                        VSTAT(2,IIF,JS) = MAX (SUMWTS(JA1,JA2,IIF,IS),
     *                     VSTAT(2,IIF,JS))
                        VSTAT(3,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS) +
     *                     VSTAT(3,IIF,JS)
                        VSTAT(4,IIF,JS) = SUMWTS(JA1,JA2,IIF,IS)**2 +
     *                     VSTAT(4,IIF,JS)
                        RSTAT(1,IIF,JS) = MIN (SUMSQW(JA1,JA2,IIF,IS),
     *                     RSTAT(1,IIF,JS))
                        RSTAT(2,IIF,JS) = MAX (SUMSQW(JA1,JA2,IIF,IS),
     *                     RSTAT(2,IIF,JS))
                        RSTAT(3,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS) +
     *                     RSTAT(3,IIF,JS)
                        RSTAT(4,IIF,JS) = SUMSQW(JA1,JA2,IIF,IS)**2 +
     *                     RSTAT(4,IIF,JS)
                        END IF
                     END IF
 10               CONTINUE
 15            CONTINUE
 20         CONTINUE
 30      CONTINUE
      DO 50 IIF = 1,NIF
         DO 40 IS = 1,NSTOK
            IF (NSTAT(IIF,IS).GT.0) THEN
               MNSTAT(3,IIF,IS) = MNSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               MNSTAT(4,IIF,IS) = MNSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               MNSTAT(4,IIF,IS) = MNSTAT(4,IIF,IS) - MNSTAT(3,IIF,IS)**2
               MNSTAT(4,IIF,IS) = SQRT (MAX (0.0, MNSTAT(4,IIF,IS)))
               MXSTAT(3,IIF,IS) = MXSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               MXSTAT(4,IIF,IS) = MXSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               MXSTAT(4,IIF,IS) = MXSTAT(4,IIF,IS) - MXSTAT(3,IIF,IS)**2
               MXSTAT(4,IIF,IS) = SQRT (MAX (0.0, MXSTAT(4,IIF,IS)))
               VSTAT(3,IIF,IS) = VSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               VSTAT(4,IIF,IS) = VSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               VSTAT(4,IIF,IS) = VSTAT(4,IIF,IS) - VSTAT(3,IIF,IS)**2
               VSTAT(4,IIF,IS) = SQRT (MAX (0.0, VSTAT(4,IIF,IS)))
               RSTAT(3,IIF,IS) = RSTAT(3,IIF,IS) / NSTAT(IIF,IS)
               RSTAT(4,IIF,IS) = RSTAT(4,IIF,IS) / NSTAT(IIF,IS)
               RSTAT(4,IIF,IS) = RSTAT(4,IIF,IS) - RSTAT(3,IIF,IS)**2
               RSTAT(4,IIF,IS) = SQRT (MAX (0.0, RSTAT(4,IIF,IS)))
               END IF
 40         CONTINUE
 50      CONTINUE
C                                       Now tell the user some stuff
      DOTX = OUTEXT.NE.' '
      IF (DOTX) THEN
         TXLUN = 3
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, OUTEXT, .TRUE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT TEXT FILE'
            CALL MSGWRT (7)
            DOTX = .FALSE.
            TXIND = 0
            END IF
      ELSE
         TXIND = 0
         END IF
C                                       summary stats
      MSGTXT = 'Minimum baseline weight averaged over all baselines'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 110 IS = 1,NSTOK
         DO 100 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (MNSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 100        CONTINUE
 110     CONTINUE
      MSGTXT = 'Maximum baesline weight averaged over all baseliness'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 130 IS = 1,NSTOK
         DO 120 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (MXSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 120        CONTINUE
 130     CONTINUE
      MSGTXT = 'Mean baseline weight averaged over all baselines'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 150 IS = 1,NSTOK
         DO 140 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (VSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 140        CONTINUE
 150     CONTINUE
      MSGTXT = 'RMS of baseline weight averaged over all baselines'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1099)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 170 IS = 1,NSTOK
         DO 160 IIF = 1,NIF
            IF (NSTAT(IIF,IS).GT.0) THEN
               WRITE (MSGTXT,1100) IIF, IS, (RSTAT(I,IIF,IS), I = 1,4)
               CALL DOMSG (5, DOTX, TXLUN, TXIND)
               END IF
 160        CONTINUE
 170     CONTINUE
C                                       outliers
      IF (BPARM(3).LE.1.0) BPARM(3) = 5.0
      MSGTXT = ' '
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      MSGTXT = 'Outliers in baseline weight'
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      WRITE (MSGTXT,1199)
      CALL DOMSG (5, DOTX, TXLUN, TXIND)
      DO 220 JS = 1,NSTOK
         IS = (JS + 1) / 2
         DO 215 IIF = 1,NIF
            DO 210 IA1 = 1,NANT-1
               DO 205 IA2 = IA1+1,NANT
                  IF (MOD(JS,2).EQ.0) THEN
                     JA1 = IA2
                     JA2 = IA1
                  ELSE
                     JA1 = IA1
                     JA2 = IA2
                     END IF
                  IF (NSUMS(JA1,JA2,IIF,IS).GT.0) THEN
                     DOIT = .FALSE.
                     X = ABS (MNWTS(JA1,JA2,IIF,IS)-MNSTAT(3,IIF,IS))
                     IF (X.GT.BPARM(3)*MNSTAT(4,IIF,IS)) THEN
                        DOIT = .TRUE.
                        MARK(1) = '*'
                     ELSE
                        MARK(1) = ' '
                        END IF
                     X = ABS (MXWTS(JA1,JA2,IIF,IS)-MXSTAT(3,IIF,IS))
                     IF (X.GT.BPARM(3)*MXSTAT(4,IIF,IS)) THEN
                        DOIT = .TRUE.
                        MARK(2) = '*'
                     ELSE
                        MARK(2) = ' '
                        END IF
                     X = ABS (SUMWTS(JA1,JA2,IIF,IS)-VSTAT(3,IIF,IS))
                     IF (X.GT.BPARM(3)*VSTAT(4,IIF,IS)) THEN
                        DOIT = .TRUE.
                        MARK(3) = '*'
                     ELSE
                        MARK(3) = ' '
                        END IF
                     X = ABS (SUMSQW(JA1,JA2,IIF,IS)-RSTAT(3,IIF,IS))
                     IF (X.GT.BPARM(3)*RSTAT(4,IIF,IS)) THEN
                        DOIT = .TRUE.
                        MARK(4) = '*'
                     ELSE
                        MARK(4) = ' '
                        END IF
                     IF (DOIT) THEN
                        WRITE (MSGTXT,1200) IA1, IA2, IIF, JS,
     *                     MNWTS(JA1,JA2,IIF,IS), MARK(1),
     *                     MXWTS(JA1,JA2,IIF,IS), MARK(2),
     *                     SUMWTS(JA1,JA2,IIF,IS), MARK(3),
     *                     SUMSQW(JA1,JA2,IIF,IS), MARK(4)
                        CALL DOMSG (5, DOTX, TXLUN, TXIND)
                        END IF
                     END IF
 205              CONTINUE
 210           CONTINUE
 215        CONTINUE
 220     CONTINUE
C                                       detailed stats
      IF ((DOTX) .OR. (BPARM(2).GE.2.0)) THEN
         M = -1
         MSGTXT = ' '
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         MSGTXT = 'All antenna weights'
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         WRITE (MSGTXT,1199)
         CALL DOMSG (M, DOTX, TXLUN, TXIND)
         DO 320 JS = 1,NSTOK
            IS = (JS + 1) / 2
            DO 315 IIF = 1,NIF
               DO 310 IA1 = 1,NANT-1
                  DO 305 IA2 = IA1+1,NANT
                     IF (MOD(JS,2).EQ.0) THEN
                        JA1 = IA2
                        JA2 = IA1
                     ELSE
                        JA1 = IA1
                        JA2 = IA2
                        END IF
                     IF (NSUMS(JA1,JA2,IIF,IS).GT.0) THEN
                        WRITE (MSGTXT,1300) IA1, IA2, IIF, JS,
     *                     MNWTS(JA1,JA2,IIF,IS), MXWTS(JA1,JA2,IIF,IS),
     *                     SUMWTS(JA1,JA2,IIF,IS),
     *                     SUMSQW(JA1,JA2,IIF,IS)
                        CALL DOMSG (M, DOTX, TXLUN, TXIND)
                        END IF
 305                 CONTINUE
 310              CONTINUE
 315           CONTINUE
 320        CONTINUE
         END IF
C                                       Close
      IF (TXIND.GT.0) CALL ZTXCLS (TXLUN, TXIND, IERR)
      IRET = 0
      IF ((OUTEXT.NE.' ') .AND. (.NOT.DOTX)) IRET = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('REPBL ERROR',I4,' ON ',A)
 1099 FORMAT ('  IF Pol      Minimum',6X,'Maximum',6X,'Mean',9X,'RMS')
 1100 FORMAT (2I4,4(F13.4))
 1199 FORMAT (' An1 An2  IF Pol     Minimum',5X,'Maximum',5X,'Mean',8X,
     *   'RMS')
 1200 FORMAT (4I4,4(F11.3,A1))
 1300 FORMAT (4I4,4F12.4)
      END

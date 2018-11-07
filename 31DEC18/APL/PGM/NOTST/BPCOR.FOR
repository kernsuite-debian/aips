LOCAL INCLUDE 'BPCOR.INC'
C                                                          Include BPCOR
C                                       Local include for BPCOR
      INCLUDE 'INCS:PUVD.INC'
C                                       Inputs and general info
      LOGICAL WNTSOU, WNTANT, INFMSG
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSTOK, XCODET,
     *   XOPCOD, XINFIL(12)
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSTOK*4, CODET*4,
     *   OPCODE*4, INFILE*48, HISCRD(30)*64
      DOUBLE PRECISION  FRQOFF(MAXIF), DTIME1, DTIME2
      REAL XSQIN, XDISIN, XFQID, XBIF, XEIF, XTIME(8), XANT(50), XSUBA,
     *   XBPVER, APARM(10), BPARM(10), CPARM(10), XBAD(10)
      INTEGER SEQIN, DISKIN, FREQID, BIF, EIF, SUBA, IBPVER, NUMHIS,
     *   CNOIN, ISTOK, ANTENS(50), NANTSL, SOUWAN(30), NSOUWD, NFRQUV
C                                       Buffers and file info
      INTEGER   BUFFER(512), BUFF2(512)
      INTEGER   BPKOLS(MAXBPC), BPNUMV(MAXBPC), BPKOL2(MAXBPC),
     *   BPNUM2(MAXBPC), IBPOVR
C                                       BP table record
      DOUBLE PRECISION DTIMBP, DFRQBP(MAXIF)
      REAL      TINTBP, BANDBP, XYBP(2,MAXCIF), WTBP(2*MAXIF), LOWSHF,
     *   DELSHF, BPFACT(MAXCHA)
      INTEGER ISOUBP, ISUBBP, IANTBP, IFQDBP, IREFBP
C                                       Internal storage
      INTEGER   NUMANT, NUMPOL, NUMIF, NUMFRQ, NFIX, IFCHAN, ILCHAN,
     *   NUMSHF
C                                       Inputs and general info
      COMMON /INPARM/ XNAMEI, XCLAIN, XSQIN, XDISIN, XXSOUR, XXSTOK,
     *   XFQID, XBIF, XEIF, XTIME, XANT, XSUBA, XBPVER, XCODET, XOPCOD,
     *   APARM, BPARM, CPARM, XINFIL, XBAD
      COMMON /BINFO/ FRQOFF, DTIME1, DTIME2, SEQIN, DISKIN, CNOIN,
     *   ISTOK, FREQID, BIF, EIF, SUBA, IBPVER, NSOUWD, SOUWAN, NANTSL,
     *   ANTENS, NUMHIS, NFRQUV,WNTSOU, WNTANT, INFMSG, BPFACT
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, XSOUR, XSTOK, CODET,
     *   OPCODE, INFILE
C                                       Buffers and file info
      COMMON /BUFRS/ BUFFER, BUFF2, BPKOLS, BPNUMV, BPKOL2, BPNUM2,
     *   IBPOVR
C                                       BP record
      COMMON /BPRECC/ DTIMBP, DFRQBP, TINTBP, BANDBP, XYBP, WTBP,
     *   ISOUBP, ISUBBP, IANTBP, IFQDBP, IREFBP(2), LOWSHF, DELSHF
C                                       Internal storage
      COMMON /BPCOM/  NUMANT, NUMPOL, NUMIF, NUMFRQ, NFIX, IFCHAN,
     *   ILCHAN, NUMSHF
C                                                          End BPCOR
LOCAL END
      PROGRAM BPCOR
C-----------------------------------------------------------------------
C! Corrects BP spectra.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2007, 2011-2012, 2018
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
C   Task BPCOR corrects BP spectra.
C-----------------------------------------------------------------------
C   Input/output via common
C      BUFFER   I(512)    Buffer.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'BPCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'BPCOR '/
C-----------------------------------------------------------------------
C                                       Get input parameters
      CALL BPCINN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Smooth the BP spectra.
      CALL BPCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy and update HI file.
      CALL BPCHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE BPCINN (PRGN, JERR)
C-----------------------------------------------------------------------
C   BPCINN gets input parameters for BPCOR.
C   Input parameter
C      PRGN    C*6        Program name
C   Output parameter
C      JERR    I          Error code: 0 => ok
C                              <>0 => Invalid request
C                                5 => catalog troubles
C                                8 => can't start
C   Input from common
C      KDCRV    D(7)       Coordinate value at ref pixel.
C      KINAX    I(7)       Number of pixels on each axis.
C      KIPCN    I          Number of random parameters.
C      KRCIC    R(7)       Coordinate value increment along axis.
C      XTIME    R(8)       Timerange to select (d,h,m,s,d,h,m,s).
C      XSUBA    R          Subarray to select.
C      CATD     D(128)     Catalog header.
C      CATR     R(256)     Catalog header.
C      ISORT    C*2        Sort order (1st char meaningful).
C      JLOCF    I          Order in data of frequency.
C      JLOCIF   I          Order in data of IF.
C      JLOCS    I          Order in data of Stokes parameters.
C   Input/output via common
C      ANTENS   I(50)      List of antennas.
C      FRQOFF   D(MAXIF)   Frequency offset (if#).
C      SOUWAN   I(30)      List of source numbers.
C      BIF      I          First IF selected (1-rel to first in data)
C      CNOIN    I          Input file catalog number.
C      DISKIN   I          Input file disk number.
C      DTIME1   D          Start time (days).
C      DTIME2   D          End time (days).
C      EIF      I          Last IF selected 0=> all.
C      FREQID   I          Frequency id selected.
C      IBPVER   I          Input BP table version number.
C      NANTSL   I          No of antennas selected.
C      NFRQUV   I          No of freq channels in the UV file.
C      NSOUWD   I          No of selected sources.
C      NUMHIS   I          No of history records in HISCRD.
C      SEQIN    I          Input file sequence number.
C      SUBA     I          Subarray number selected.
C      WNTANT   L          False if antennas de-selected.
C      WNTSOU   L          False if sources de-selected.
C      NUMIF    I          No of IF's in UV file.
C      BUFFER   I(512)   Buffer.
C      FCNO     I(128)     Catalog slot nos of the marked files.
C      FRW      I(128)     R/W flags for marked catalog files.
C      FVOL     I(128)     Disk nos of the marked cat files.
C      NCFILE   I          No of catalog files marked.
C      NSCR     I          No of scratch files created.
C      RQUICK   L          Restart AIPS ?
C      XSOUR    C(30)*16   Source names.
C      CLAIN    C*6        Input file class.
C      CODET    C*4        Data type to smooth (R&I,A&P).
C      NAMEIN   C*12       Input file name.
C      OPCODE   C*4        Smoothing type (HANN,GAUS etc).
C      XSTOK    C*4        Stokes type to select.
C      XANT     R(50)      Antennas to select.
C      XBAD     R(10)      Disk numbers to be avoided for scratch.
C      XCLAIN   H(2)       Input file class.
C      XNAMEI   H(3)       Input file name.
C      XXSOUR   H(4,30)    Source names.
C      XBIF     R          Start IF to select.
C      XBPVER   R          Input BP table version number.
C      XCODET   H          Data type to smooth (R&I,A&P).
C      XDISIN   R          Input file disk number.
C      XEIF     R          End IF to select.
C      XFQID    R          Freq. id to select.
C      XOPCOD   H          Smoothing type (HANN,GAUS etc).
C      XSQIN    R          Input file sequence number.
C      XXSTOK   H          Stokes type to select eg.'RR','LL','RL'
C      CATBLK   I(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C      NLUSER   I          User number.
C      BANDW    D          Bandwidth in catalog header.
C      NRPARM   I          Number of random parameters.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      LOGICAL   T, F, ALLANT, DESEL, ISMULT
      INTEGER   NPARM, IERR, I, NEXT, IARG, LIMIT, J, IROUND, LUN, IIVER
      INCLUDE 'BPCOR.INC'
      INTEGER   DUMMY(MAXIF)
      REAL      RDUMM(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /29/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 245
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSQIN)
      DISKIN = IROUND (XDISIN)
      SUBA = XSUBA + 0.5
      IF (SUBA.LE.0) SUBA = 1
      IBPVER = IROUND (XBPVER)
      DO 20 I = 1,10
         IBAD(I) = IROUND (XBAD(I))
 20      CONTINUE
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (4, 1, XXSTOK, XSTOK)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      CALL H2CHR (4, 1, XCODET, CODET)
      DO 25 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 25      CONTINUE
      CALL H2CHR (48, 1, XINFIL, INFILE)

C                                       Find file, read CATBLK
      CNOIN = 1
      STAT = 'SRCH'
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'WRIT', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      NRPARM = CATBLK(KIPCN)
C                                       Get observing bandwidth
      BANDW  = CATBLK(KINAX+JLOCF) * CATR(KRCIC+JLOCF)
C                                       Freq id
      FREQID = IROUND (XFQID)
C                                       Timerange
      DTIME1 = XTIME(1) + XTIME(2) / 24.0 + XTIME(3) / (24.0*60.0) +
     *   (XTIME(4) / (24.0*60.0*60.0))
      DTIME2 = XTIME(5) + XTIME(6) / 24.0 + XTIME(7) / (24.0*60.0) +
     *   (XTIME(8) / (24.0*60.0*60.0))
      IF ((DTIME2.LT.DTIME1) .OR. (DTIME2.LT.1.0E-5)) DTIME2 = 1.0E20
C                                       No of freq channels in UV data
      NFRQUV = 0
      IF (JLOCF.GT.0) NFRQUV = CATBLK(KINAX+JLOCF)
C                                       IF range
      BIF = IROUND (XBIF)
      EIF = IROUND (XEIF)
      IF (BIF.LE.0) BIF = 1
      IF ((EIF.LE.0) .AND. (JLOCIF.GT.0)) EIF = CATBLK(KINAX+JLOCIF)
      IF (EIF.LE.0) EIF = 1
      IF ((JLOCIF.GT.0) .AND. (BIF.GT.CATBLK(KINAX+JLOCIF)))
     *   BIF = CATBLK(KINAX+JLOCIF)
      IF ((JLOCIF.GT.0) .AND. (EIF.GT.CATBLK(KINAX+JLOCIF)))
     *   EIF = CATBLK(KINAX+JLOCIF)
C                                       Stokes' type.
      ISTOK = 0
      IF (XSTOK.EQ.'R   ') ISTOK = 1
      IF (XSTOK.EQ.'L   ') ISTOK = 2
C                                       Check Stokes'
      IF (ISTOK.EQ.0) THEN
C                                       If none selected take what you
C                                       have.
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +1.0D0) .LE. 0.5D0)) ISTOK = 1
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND. (ABS (CATD(KDCRV+JLOCS)
     *      +2.0D0) .LE. 0.5D0)) ISTOK = 2
      ELSE
C                                       Is selected Stokes' available?
         IF ((CATBLK(KINAX+JLOCS).EQ.1) .AND.
     *      (ABS (CATD(KDCRV+JLOCS)+ISTOK).GT.0.5D0)) THEN
            JERR = 1
            MSGTXT = 'STOKES ' // XSTOK // ' UNAVAILABLE IN DATA'
            GO TO 990
            END IF
         END IF
C                                       Check sort order of input
      IF (ISORT(1:2).NE.'TB') THEN
         WRITE (MSGTXT,1060) ISORT
         JERR = 1
         GO TO 990
         END IF
      JERR = 0
C                                       Antenna list
      ALLANT = T
      DESEL = F
      DO 100 I = 1,50
         ANTENS(I) = 0
         ALLANT = ALLANT .AND. (ABS (XANT(I)).LE.1.0E-10)
         DESEL = DESEL .OR. (XANT(I).LT.-0.5)
 100     CONTINUE
      NEXT = 1
      IF (ALLANT) GO TO 160
C                                       Not all selected - make list
C                                       ANTENNAS array.
         DO 150 I = 1,50
            IARG = ABS (XANT(I)) + 0.5
            IF (IARG.EQ.0) GO TO 150
C                                       See if already have
               LIMIT = NEXT - 1
               IF (LIMIT.LT.1) GO TO 140
               DO 130 J = 1,LIMIT
                  IF (IARG.EQ.ANTENS(J)) GO TO 150
 130              CONTINUE
C                                       New antenna
 140              ANTENS(NEXT) = IARG
                  NEXT = NEXT + 1
 150           CONTINUE
 160  WNTANT = .NOT. DESEL
      NANTSL = NEXT - 1
      INFMSG = .FALSE.
C                                       Get source numbers
      CALL MULSDB (CATBLK, ISMULT)
      IF (ISMULT) THEN
         CALL FNDSOU (DISKIN, CNOIN, XSOUR, BUFFER, NSOUWD, WNTSOU,
     *      SOUWAN, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
         NSOUWD = 0
         END IF
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBA, CATBLK, BUFFER, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Get IF information
      IIVER = 1
      CALL CHNDAT ('READ', BUFFER, DISKIN, CNOIN, IIVER, CATBLK, LUN,
     *   NUMIF, FRQOFF, DUMMY, RDUMM, BNDCOD, FREQID, JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Ensure CODET is correct
      IF (OPCODE.EQ.'SPEC') CODET = 'A&P'
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPCINN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('INPUT VIS RECORDS MISORDERED, SORTED = ',A2,
     *        ' SHOULD BE = TB')
      END
      SUBROUTINE BPCUV (IERR)
C-----------------------------------------------------------------------
C   BPCUV reads through the input BP table, passes the selected
C   records to the appropriate correction routine, and copies
C   each record to the output BP table.
C   Output parameter
C      IERR     I          Return code, 0=OK, else failed
C   Input from common
C      ANTENS   I(50)      List of antennas.
C      SOUWAN   I(30)      List of source numbers.
C      DTIME1   D          Start time (days).
C      DTIME2   D          End time (days).
C      FREQID   I          Frequency id selected.
C      NANTSL   I          No of antennas selected.
C      NFRQUV   I          No of freq channels in the UV file.
C      NSOUWD   I          No of selected sources.
C      SUBA     I          Subarray number selected.
C      WNTANT   L          False if antennas de-selected.
C      WNTSOU   L          False if sources de-selected.
C   Input/output via common
C      IBPOVR   I          Output BP table version no.
C      CNOIN    I          Input file catalog number.
C      DISKIN   I          Input file disk number.
C      IBPVER   I          Input BP table version number.
C      NFIX     I          No of BP records modified.
C      NUMANT   I          No of antennnas.
C      NUMFRQ   I          No of freq channels in UV file.
C      NUMIF    I          No of IF's in UV file.
C      NUMPOL   I          No of polzns in UV file.
C      DFRQBP   D(MAXIF)   Ref freq (if#).
C      IREFBP   I(2)       Ref ant in BP table (polzn#).
C      XYBP     C(*,*,*)   Complex bandpass (channel#, if#, polzn#).
C      WTBP     R(*,*)     Weights (if#, polzn#).
C      BANDBP   R          Bandwidth of BP table channel.
C      DTIMBP   D          Center time of BP record.
C      IANTBP   I          Antenna no in BP table.
C      IFQDBP   I          FQ ID in BP table.
C      ISOUBP   I          Source ID no in BP table.
C      ISUBBP   I          Subarray no in BP table.
C      TINTBP   R          Time interval of BP record.
C      BPKOL2   I(MAXBPC)  BP column pointers (output).
C      BPKOLS   I(MAXBPC)  BP column pointers (input).
C      BPNUM2   I(MAXBPC)  BP element count in each column (output).
C      BPNUMV   I(MAXBPC)  BP element count in each column (input).
C      BUFF2    I(512)   Buffer.
C      BUFFER   I(512)   Buffer.
C      CATBLK   I(256)     Catalog header.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      LOGICAL   WFOUND
      CHARACTER LBPKEY*8
      INTEGER   LUN1, LUN2, IBPRNO, IBPCHN, NUMREC, LOOP, J, IBPOUT,
     *   IRCODE, JERR, IPBAD, I, NN
      INCLUDE 'BPCOR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUN1, LUN2 /29, 30/
C-----------------------------------------------------------------------
      NFIX = 0
C                                       Reformat table ?
      CALL BPREFM (DISKIN, CNOIN, IBPVER, CATBLK, LUN1, IERR)
      IF (IERR .NE. 0) GO TO 999
C                                       Open input BP table
      CALL BPINI ('READ', BUFFER, DISKIN, CNOIN, IBPVER, CATBLK, LUN1,
     *   IBPRNO, BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, IBPCHN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check for polynomial BP
      CALL CHBLNK (8, 1, LBPKEY, NN)
      IF (NN.NE.0) THEN
         IERR = 9
         WRITE (MSGTXT,1002)
         CALL MSGWRT (6)
C                                       Close table
         IRCODE = 0
         CALL TABIO ('CLOS', IRCODE, IBPRNO, BUFFER, BUFFER, IERR)
         GO TO 999
         END IF
C
      WRITE (MSGTXT,1005) IBPVER
      CALL MSGWRT (6)
C                                       Init the function
      IF (OPCODE.EQ.'FUNC') THEN
         CALL BPCORF ('INIT', NUMPOL, NUMIF, NUMFRQ, INFILE, BPFACT,
     *      XYBP, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       Get number of records
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
      IRCODE = 0
C                                       Warn if no of freq channels
C                                       disagrees with UV header.
      IF (NUMFRQ.NE.NFRQUV) THEN
         WRITE (MSGTXT,1000) NUMFRQ, NFRQUV
         CALL MSGWRT (6)
         END IF
C                                       Open output BP table
      IBPOVR = 0
      CALL BPINI ('WRIT', BUFF2, DISKIN, CNOIN, IBPOVR, CATBLK, LUN2,
     *   IBPOUT, BPKOL2, BPNUM2, NUMANT, NUMPOL, NUMIF, NUMFRQ, IBPCHN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IERR)
      IF (IERR .NE. 0) GO TO 999
      WRITE (MSGTXT,1010) IBPOVR
      CALL MSGWRT (6)
C                                       Update table
      DO 500 LOOP = 1,NUMREC
         IBPRNO = LOOP
         CALL TABBP ('READ', BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, DTIMBP, TINTBP, ISOUBP, ISUBBP, IANTBP,
     *      BANDBP, DFRQBP, IFQDBP, IREFBP, WTBP, XYBP, IERR)
         IF (IERR.GT.0) GO TO 900
C                                       Skip if both polzn's flagged
         IF (IERR.LE.-3) GO TO 450
         IPBAD = ABS (IERR)
C                                       Is this record selected ?
C                                       Stokes
         IF ((IPBAD.GT.0) .AND. (IPBAD.EQ.ISTOK)) GO TO 450
         IF ((IPBAD.EQ.0) .AND. (ISTOK.GT.0)) IPBAD = -ISTOK + 3
C                                       Time
         IF ((DTIMBP.LT.DTIME1) .OR. (DTIMBP.GT.DTIME2)) GO TO 450
C                                       Subarray
         IF (ISUBBP.NE.SUBA) GO TO 450
C                                       Freq id
         IF ((IFQDBP.NE.FREQID) .AND. (FREQID.GT.0) .AND. (IFQDBP.GT.0))
     *      GO TO 450
C                                       Check source list
         IF (NSOUWD.LE.0) GO TO 70
         DO 60 J = 1,NSOUWD
            IF ((ISOUBP.EQ.SOUWAN(J)) .AND. WNTSOU) GO TO 70
            IF ((ISOUBP.EQ.SOUWAN(J)) .AND. (.NOT.WNTSOU)) GO TO 450
 60         CONTINUE
         IF (WNTSOU) GO TO 450
C                                       See if all antennas desired
 70      IF (NANTSL.GT.0) THEN
            WFOUND = .FALSE.
            DO 120 I = 1,NANTSL
               WFOUND = WFOUND .OR. (IANTBP.EQ.ANTENS(I))
 120           CONTINUE
C                                       Check for match selected.
            IF (WNTANT .AND. WFOUND) GO TO 130
C                                       Check for match excluded
            IF (.NOT.WNTANT .AND. WFOUND) GO TO 450
C                                       If inclusion ignore
            IF (WNTANT) GO TO 450
            END IF
C                                       Smooth BP record
 130     IF (OPCODE.EQ.'FUNC') THEN
            CALL BPCORF ('APPL', NUMPOL, NUMIF, NUMFRQ, INFILE, BPFACT,
     *         XYBP, JERR)
         ELSE
            CALL BPCORR (2, IPBAD, JERR)
            END IF
C                                       Copy record to output file.
 450     CALL TABBP ('WRIT', BUFF2, IBPOUT, BPKOL2, BPNUM2, NUMIF,
     *      NUMFRQ, NUMPOL, DTIMBP, TINTBP, ISOUBP, ISUBBP, IANTBP,
     *      BANDBP, DFRQBP, IFQDBP, IREFBP, WTBP, XYBP, IERR)
         IF (IERR.GT.0) GO TO 900
C                                       End loop
 500     CONTINUE
C                                       Final call to BPCORR
      CALL BPCORR (3, IPBAD, JERR)
C                                       Close tables.
      IRCODE = 0
      CALL TABIO ('CLOS', IRCODE, IBPRNO, BUFFER, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 910
      CALL TABIO ('CLOS', IRCODE, IBPOUT, BUFF2, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 920
      GO TO 999
C                                       TABBP error
 900  WRITE (MSGTXT,1900) IERR
      GO TO 990
C                                       TABIO close errors.
 910  WRITE (MSGTXT,1910) IERR
      GO TO 990
 920  WRITE (MSGTXT,1920) IERR
      GO TO 990
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NO OF BP CHAN:',I5,' UV CHAN:',I5,
     * ' - NORMALISE WITH CARE')
 1002 FORMAT ('CANNOT PROCESS POLYNOMIAL BANDPASS SOLUTIONS')
 1005 FORMAT ('Reading BP table version no: ',I4)
 1010 FORMAT ('Writing BP table version no: ',I4)
 1900 FORMAT ('TABBP ERROR',I3,' SMOOTHING BP TABLE')
 1910 FORMAT ('TABIO ERROR',I3,' CLOSING INPUT BP TABLE')
 1920 FORMAT ('TABIO ERROR',I3,' CLOSING OUTPUT BP TABLE')
      END
      SUBROUTINE BPCORR (IOP, IPBAD, IERR)
C-----------------------------------------------------------------------
C   BPCORR is the control routine for the corrections to be applied.
C   Depending on the string in OPCODE it will call one of several(?)
C   correction routines.
C   Input parameters:
C    IOP        I    Operation code, 3=finish, <> 3=process
C    IPBAD      I    Number of polarizations flagged
C                    0=none; 1=pol#1; 2=pol#2.
C   Output parameters:
C    IERR       I    Error code, 0=OK, else failed.
C   Input from common
C      OPCODE   C*4        Correction type (HANN,GAUS etc).
C-----------------------------------------------------------------------
      INTEGER   IOP, IPBAD, IERR
C
      INCLUDE 'BPCOR.INC'
C-----------------------------------------------------------------------
C                                       Smooth or filter
      IF ((OPCODE.EQ.'HANN').OR.(OPCODE.EQ.'GAUS').OR.
     *   (OPCODE.EQ.'BOXC').OR.(OPCODE.EQ.'SINC') .OR.
     *   (OPCODE.EQ.'BMED') .OR. (OPCODE.EQ.' '))
     *   CALL BPSMTH (IOP, IPBAD, IERR)
C                                       Spectral index
      IF (OPCODE.EQ.'SPEC') CALL BPSPEC (IOP, IPBAD, IERR)
C
 999  RETURN
      END
      SUBROUTINE BPSMTH (IOP, IPBAD, IERR)
C-----------------------------------------------------------------------
C   BPSMTH smooths the BP record passed thru common /BPRECC/.
C   Input parameters:
C    IOP        I    Operation code, 3=finish, <> 3=process
C    IPBAD      I    Number of polarizations flagged
C                    0=none; 1=pol#1; 2=pol#2.
C   Output parameters:
C    IERR       I    Error code, 0=OK, else failed.
C   Input from common
C      NUMIF    I          No of IF's in UV file.
C      CODET    C*4        Data type to smooth (R&I,A&P).
C      BPARM    R(10)      AIPS input parameter values.
C   Input/output via common
C      NUMHIS   I          No of history records in HISCRD.
C      NFIX     I          No of BP records modified.
C      NUMFRQ   I          No of freq channels in UV file.
C      XYBP     R(*,*,*)   Complex bandpass (channel#, if#, polzn#).
C      HISCRD   C(30)*64   History records.
C      OPCODE   C*4        Smoothing type (HANN,GAUS etc).
C      FBLANK   R          REAL value indicating blanking.
C      APARM    R(10)      AIPS input parameter values.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   IOP, IPBAD, IERR
C
      INCLUDE 'BPCOR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER   MXSUPT
      PARAMETER (MXSUPT = 2*MAXCHA)
      REAL      X, Y, VIS(2,MAXCHA), VWORK(2,MAXCHA), VWORK2(MAXCHA),
     *   AMPL, PHSE, CDIAM, CSUPRT, DDIAM(4), DSUPRT(4)
      INTEGER   IPOL, IFNO, ICHAN, NDIAM, ISUPRT, NXSUPT, INX, LNX
      DATA NXSUPT /MXSUPT/
      DATA DDIAM  /4.0, 2.0, 2.0, 3.0/
      DATA DSUPRT /2.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Determine operation
      IF (IOP.EQ.3) GO TO 900
C                                       Smooth BP record
C                                       Loop through each polzn, IF
      DO 650 IPOL = 1,NUMPOL
C                                       Skip if no data for this polzn
C                                       or not selected.
         IF (IPOL.EQ.IPBAD) GO TO 650
         INX = (IPOL - 1) * NUMFRQ * NUMIF
         LNX = INX
         DO 600 IFNO = 1,NUMIF
            DO 100 ICHAN = 1,NUMFRQ
               INX = INX + 1
               X = XYBP(1,INX)
               Y = XYBP(2,INX)
               VIS(1,ICHAN) = X
               VIS(2,ICHAN) = Y
               IF ((X.EQ.FBLANK) .OR. (Y.EQ.FBLANK)) GO TO 100
C                                       Select data type to be smoothed
C                                       Default: Real and imaginary
               IF (CODET.EQ.'A&P') THEN
C                                       Amplitude and phase
                  VIS(1,ICHAN) = SQRT (X*X + Y*Y)
                  VIS(2,ICHAN) = 0.0
                  IF (X.NE.0) VIS(2,ICHAN) = ATAN2(Y,X)
                  END IF
 100           CONTINUE
C                                       Select the type of smoothing
C                                       1: Convolution smoothing
            IF ((OPCODE.EQ.'HANN').OR.(OPCODE.EQ.'GAUS').OR.
     *         (OPCODE.EQ.'BOXC').OR.(OPCODE.EQ.'SINC')) THEN
               CDIAM = APARM(1)
               CSUPRT = APARM(2)
C                                       Provide defaults
               IF (CDIAM.LE.0.1) THEN
                  IF (OPCODE.EQ.'HANN') CDIAM = DDIAM(1)
                  IF (OPCODE.EQ.'GAUS') CDIAM = DDIAM(2)
                  IF (OPCODE.EQ.'BOXC') CDIAM = DDIAM(3)
                  IF (OPCODE.EQ.'SINC') CDIAM = DDIAM(4)
                  END IF
               IF (CSUPRT.LT.CDIAM) THEN
                  IF (OPCODE.EQ.'HANN') CSUPRT = CDIAM * DSUPRT(1)
                  IF (OPCODE.EQ.'GAUS') CSUPRT = CDIAM * DSUPRT(2)
                  IF (OPCODE.EQ.'BOXC') CSUPRT = CDIAM * DSUPRT(3)
                  IF (OPCODE.EQ.'SINC') CSUPRT = CDIAM * DSUPRT(4)
                  END IF
               ISUPRT = CSUPRT
               CSUPRT = MIN (ISUPRT, NXSUPT)
               IF (.NOT.INFMSG) THEN
                  INFMSG = .TRUE.
                  WRITE (MSGTXT,1000) OPCODE
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1010) CDIAM
                  CALL MSGWRT (5)
                  WRITE (MSGTXT,1020) CSUPRT
                  CALL MSGWRT (5)
                  END IF
               CALL SCONVL (OPCODE, CDIAM, CSUPRT, VIS, VWORK, NUMFRQ,
     *            FBLANK, IERR)
               END IF
C                                       2: Median smoothing
            IF (OPCODE.EQ.'BMED') THEN
               NDIAM = APARM(1) + 0.1
               CALL MEDWIN (OPCODE, NDIAM, VIS, VWORK, VWORK2, NUMFRQ,
     *            FBLANK, IERR)
               END IF
C                                       Convert to ampl and phase
            IF (CODET.NE.'A&P') THEN
               DO 200 ICHAN = 1,NUMFRQ
                  X = VIS(1,ICHAN)
                  Y = VIS(2,ICHAN)
                  IF ((X.EQ.FBLANK).OR.(Y.EQ.FBLANK)) GO TO 200
                  VIS(1,ICHAN) = SQRT(X*X + Y*Y)
                  VIS(2,ICHAN) = 0.0
                  IF (X.NE.0) VIS(2,ICHAN) = ATAN2(Y,X)
 200              CONTINUE
               END IF
C                                       Set phase to zero ?
            IF (BPARM(1).EQ.1.0) THEN
               DO 300 ICHAN = 1,NUMFRQ
                  IF ((VIS(1,ICHAN).NE.FBLANK).AND.
     *               (VIS(2,ICHAN).NE.FBLANK)) VIS(2,ICHAN) = 0
 300              CONTINUE
               END IF
C                                       Normalise BP spectrum ampl ?
            IF (BPARM(2).LE.0.0) CALL NRMLBP (VIS, NUMFRQ, BPARM(3),
     *         FBLANK)
C                                       Convert back to (real,imag)
            DO 400 ICHAN = 1,NUMFRQ
               LNX = LNX + 1
               IF ((VIS(1,ICHAN).NE.FBLANK) .AND.
     *            (VIS(2,ICHAN).NE.FBLANK)) THEN
                  AMPL = VIS(1,ICHAN)
                  PHSE = VIS(2,ICHAN)
                  XYBP(1,LNX) = AMPL * COS (PHSE)
                  XYBP(2,LNX) = AMPL * SIN (PHSE)
                  END IF
 400           CONTINUE
 600        CONTINUE
 650     CONTINUE
C                                       Update no of records modified
      NFIX = NFIX + 1
      GO TO 999
C                                       Finish - number changed.
 900  NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2900) NFIX
      WRITE (MSGTXT,2900) NFIX
      CALL MSGWRT (6)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Smoothing BP spectra with a ',A4,' function')
 1010 FORMAT ('Function diameter = ',F5.0,' channels')
 1020 FORMAT ('Function support over ',F5.0,' channels')
 2900 FORMAT (I6,' Records modified')
      END
      SUBROUTINE BPSPEC (IOP, IPBAD, IERR)
C-----------------------------------------------------------------------
C   BPSPEC corrects the BP record passed thru common /BPRECC/ for a
C   spectral index given by APARM(1)
C   Input parameters:
C    IOP        I    Operation code, 3=finish, <> 3=process
C    IPBAD      I    Number of polarizations flagged
C                    0=none; 1=pol#1; 2=pol#2.
C   Output parameters:
C    IERR       I    Error code, 0=OK, else failed.
C   Input from common
C      NUMIF    I          No of IF's in UV file.
C      APARM    R(10)      AIPS input parameter values.
C      BPARM    R(10)      AIPS input parameter values.
C   Input/output via common
C      NUMHIS   I          No of history records in HISCRD.
C      NFIX     I          No of BP records modified.
C      NUMFRQ   I          No of freq channels in UV file.
C      XYBP     C(*,*,*)   Complex bandpass (channel#, if#, polzn#).
C      HISCRD   C(30)*64   History records.
C      OPCODE   C*4        Smoothing type (HANN,GAUS etc).
C      FBLANK   R          REAL value indicating blanking.
C      APARM    R(10)      AIPS input parameter values.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      INTEGER   IOP, IPBAD, IERR
C
      INCLUDE 'BPCOR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      REAL      X, Y, VIS(2,MAXCHA), AMPL, PHSE, ACOR, REFPIX, CHNINC,
     *   CHNPIV, FINC(MAXIF)
      INTEGER   IPOL, IFNO, ICHAN, IROUND, INX, LNX, ISBAND(MAXIF),
     *   FQBUFF(512), FQLUN, LUNTMP, VER, NIF, FQID
      DOUBLE PRECISION REFFRQ, CHNOFQ, FOFF(MAXIF), CHNFQ
      CHARACTER BNDCOD(MAXIF)*8
C-----------------------------------------------------------------------
      IERR = 0
      FQLUN = LUNTMP (1)
      VER = 1
      CALL CHNDAT ('READ', FQBUFF, DISKIN, CNOIN, VER, CATBLK, FQLUN,
     *   NIF, FOFF, ISBAND, FINC, BNDCOD, FQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'GETTING FREQ PARAMETERS'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Determine operation
      IF (IOP.EQ.3) GO TO 900
      IFCHAN = IROUND(APARM(2))
      ILCHAN = IROUND(APARM(3))
      CHNPIV = APARM(4)
      IF (IFCHAN.LE.0) IFCHAN = 1
      IF (ILCHAN.EQ.0) ILCHAN = NUMFRQ
      IF (IFCHAN.GT.ILCHAN) IFCHAN = 1
      IF (ILCHAN.GT.NUMFRQ) ILCHAN = NUMFRQ
      IF (CHNPIV.EQ.0.0) CHNPIV = NUMFRQ/2.0 + 1.0
      REFFRQ = CATD(KDCRV+JLOCF)
      CHNINC = CATR(KRCIC+JLOCF)
      REFPIX = CATR(KRCRP+JLOCF)
      CHNOFQ = REFFRQ + (REFPIX - CHNPIV) * CHNINC
C                                       Check BANDBP >
C                                       Correct BP record
C                                       Loop through each polzn, IF
      DO 650 IPOL = 1,NUMPOL
C                                       Skip if no data for this polzn
C                                       or not selected.
         IF (IPOL.EQ.IPBAD) GO TO 650
         INX = (IPOL - 1) * NUMIF * NUMFRQ
         LNX = INX
         DO 600 IFNO = 1,NUMIF
            CHNINC = FINC(IFNO)
            CHNOFQ = REFFRQ + FOFF(IFNO) + (CHNPIV-REFPIX) * CHNINC
            DO 100 ICHAN = 1,NUMFRQ
               INX = INX + 1
               X = XYBP(1,INX)
               Y = XYBP(2,INX)
               IF ((X.EQ.FBLANK) .OR. (Y.EQ.FBLANK)) GO TO 100
C                                       Amplitude
               VIS(1,ICHAN) = SQRT (X*X + Y*Y)
               VIS(2,ICHAN) = 0.0
               IF ((X.NE.0) .OR. (Y.NE.0.0)) VIS(2,ICHAN) = ATAN2(Y,X)
 100           CONTINUE
C                                       Correct for spectral index,
C                                       start at channel 2
            DO 150 ICHAN = 1,NUMFRQ
               CHNFQ = REFFRQ + FOFF(IFNO) + (ICHAN-REFPIX) * CHNINC
               ACOR = (CHNFQ/CHNOFQ) ** (APARM(1)/2.0)
               IF (ACOR.EQ.0.0) ACOR = 1.0
               IF ((ICHAN.GE.IFCHAN) .AND. (ICHAN.LE.ILCHAN)) THEN
                  VIS(1,ICHAN) = VIS(1,ICHAN) / ACOR
                  END IF
 150           CONTINUE
C                                       Set phase to zero ?
            IF (BPARM(1).EQ.1.0) THEN
               DO 300 ICHAN = 1,NUMFRQ
                  IF ((VIS(1,ICHAN).NE.FBLANK).AND.
     *               (VIS(2,ICHAN).NE.FBLANK)) VIS(2,ICHAN) = 0
 300              CONTINUE
               END IF
C                                       Normalise BP spectrum ampl ?
            IF (BPARM(2).LE.0.0) CALL NRMLBP (VIS, NUMFRQ, BPARM(3),
     *         FBLANK)
C                                       Convert back to (real,imag)
            DO 400 ICHAN = 1,NUMFRQ
               LNX = LNX + 1
               IF ((VIS(1,ICHAN).NE.FBLANK) .AND.
     *            (VIS(2,ICHAN).NE.FBLANK)) THEN
                  AMPL = VIS(1,ICHAN)
                  PHSE = VIS(2,ICHAN)
                  XYBP(1,LNX) = AMPL * COS (PHSE)
                  XYBP(2,LNX) = AMPL * SIN (PHSE)
                  END IF
 400           CONTINUE
 600        CONTINUE
 650     CONTINUE
C                                       Update no of records modified
      NFIX = NFIX + 1
      GO TO 999
C                                       Finish - number changed.
 900  NUMHIS= NUMHIS + 1
      WRITE (HISCRD(NUMHIS),2900) NFIX
      WRITE (MSGTXT,2900) NFIX
      CALL MSGWRT (6)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPSPEC ERROR',I5,' ON ',A)
 2900 FORMAT (I6,' Records modified')
      END
      SUBROUTINE BPCHI
C-----------------------------------------------------------------------
C   BPCHI copies and updates the history file.
C   Input from common
C      IBPOVR   I          Output BP table version.
C      ANTENS   I(50)      List of antennas.
C      BIF      I          First IF selected (1-rel to first in data)
C      DTIME1   D          Start time (days).
C      DTIME2   D          End time (days).
C      EIF      I          Last IF selected 0=> all.
C      IBPVER   I          Input BP table version number.
C      NSOUWD   I          No of selected sources.
C      NUMHIS   I          No of history records in HISCRD.
C      SUBA     I          Subarray number selected.
C      WNTANT   L          False if antennas de-selected.
C      WNTSOU   L          False if sources de-selected.
C      NCFILE   I          No of catalog files marked.
C      HISCRD   C(30)*64   History records.
C      XSOUR    C(30)*16   Source names.
C      CODET    C*4        Data type to smooth (R&I,A&P).
C      OPCODE   C*4        Correction type (HANN,GAUS etc).
C      XSTOK    C*4        Stokes type to select.
C      RLSNAM   C*8        Release name.
C      APARM    R(10)      AIPS input parameter values.
C      BPARM    R(10)      AIPS input parameter values.
C      TSKNAM   C*6        AIPS task name.
C   Input/output via common
C      DISKIN   I          Input file disk number.
C      NANTSL   I          No of antennas selected.
C      BUFFER   I(512)     Buffer.
C      FCNO     I(128)     Catalog slot nos of the marked files.
C      MSGTXT   C*80       AIPS message string.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, CTIME(2)*12, LABEL*8, LANS1, LANS2
      INTEGER   LUN1, IERR, I, TIME(3), DATE(3), LIMIT, LIMIT2, J
      REAL      TIMBEG, TIMEND
      LOGICAL   T
      INCLUDE 'BPCOR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), BUFFER, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 100
C                                       Task message
 10   CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1010) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Sources
         IF (NSOUWD.LE.0) THEN
            WRITE (HILINE,3000) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         ELSE
C                                       Included or excluded?
            WRITE (HILINE,3001) TSKNAM
            IF (WNTSOU) WRITE (HILINE,3002) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       1st 2 and label.
            WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            IF (NSOUWD.LE.2) GO TO 25
C                                       Rest of sources
            DO 20 I = 3,NSOUWD,2
               WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
               CALL HIADD (LUN1, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
 20            CONTINUE
            END IF
C                                       Antennas
 25      IF (NANTSL.LE.0) THEN
            WRITE (HILINE,3005) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         ELSE
C                                       Included or excluded?
            WRITE (HILINE,3006) TSKNAM
            IF (WNTANT) WRITE (HILINE,3007) TSKNAM
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
C                                       1st 12 and label.
            LIMIT = MIN (12, NANTSL)
            WRITE (HILINE,3008) TSKNAM, (ANTENS(J),J=1,LIMIT)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
            IF (NANTSL.LE.12) GO TO 35
C                                       Rest of antennas
            DO 30 I = 13,NANTSL,12
               LIMIT = I
               LIMIT2 = I + 11
               LIMIT2 = MIN (NANTSL, LIMIT2)
               WRITE (HILINE,3009) TSKNAM, (ANTENS(J),J=LIMIT,LIMIT2)
               CALL HIADD (LUN1, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
 30            CONTINUE
            END IF
C                                       Timerange
 35   TIMBEG = DTIME1
      TIMEND = DTIME2
      IF ((TIMEND.LT.TIMBEG) .OR. (TIMEND.LT.1.0E-5)) TIMEND = 1.0E20
      CALL HITIME (TIMBEG, TIMEND, LUN1, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Stokes'
      WRITE (HILINE,2005) TSKNAM, XSTOK
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       IF range
      WRITE (HILINE,2004) TSKNAM, BIF, EIF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Subarray, BP version
      WRITE (HILINE,2002) TSKNAM, SUBA, IBPVER, IBPOVR
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Smooth (Re,Im) or (Ampl,phse)?
      WRITE (HILINE,3010) TSKNAM
      IF (CODET.EQ.'A&P') WRITE (HILINE,3015) TSKNAM
      IF (OPCODE.EQ.'SPEC') WRITE (HILINE,3016) TSKNAM
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Smoothing type, width, support
      IF ((OPCODE.EQ.'HANN').OR.(OPCODE.EQ.'GAUS').OR.
     *   (OPCODE.EQ.'BOXC').OR.(OPCODE.EQ.'SINC') .OR.
     *   (OPCODE.EQ.'BMED')) THEN
         WRITE (HILINE,3020) TSKNAM, OPCODE, APARM(1), APARM(2)
      ELSE IF (OPCODE.EQ.'SPEC') THEN
         WRITE (HILINE,3021) TSKNAM, OPCODE, APARM(1)
         CALL HIADD (LUN1, HILINE, BUFFER, IERR)
         WRITE (HILINE,3022) TSKNAM, IFCHAN, ILCHAN
         END IF
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                       BP normalisation; phase=0
      LANS1 = 'N'
      IF (BPARM(1).EQ.1) LANS1 = 'Y'
      LANS2 = 'Y'
      IF (BPARM(2).NE.0) LANS2 = 'N'
      WRITE (HILINE,3030) TSKNAM, LANS1, LANS2
      CALL HIADD (LUN1, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 100
C                                      Add any other history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1011) TSKNAM
         HILINE(1:8) = LABEL(1:8)
         DO 40 I = 1,NUMHIS
            HILINE(9:72) = HISCRD(I)(1:64)
            CALL HIADD (LUN1, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 100
 40         CONTINUE
         END IF
C                                       FUNC
      IF (OPCODE.EQ.'FUNC') THEN
         DO 50 I = 1,NUMFRQ
            IF (ABS(BPFACT(I)-1.0).GT.0.0001) THEN
               WRITE (HILINE,1040) TSKNAM, I, BPFACT(I)
               CALL HIADD (LUN1, HILINE, BUFFER, IERR)
               IF (IERR.NE.0) GO TO 100
               END IF
 50         CONTINUE
         END IF
C                                       Close HI file
 100  CALL HICLOS (LUN1, T, BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPCHI: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,'RELEASE =''',A7,' ''  /********* Start ',
     *   A12,2X,A8)
 1011 FORMAT (A6,'  ')
 1040 FORMAT (A6,'BPFACT(',I4,')=',F9.5)
 2002 FORMAT (A6,'SUBARRAY =',I3,' BPVER = ',I4,' BP out = ',I4)
 2004 FORMAT (A6,'BIF =',I4,', EIF =',I4,'/ IF range')
 2005 FORMAT (A6,'STOKES = ''',A4,''' / Stokes type')
 3000 FORMAT (A6,'SOURCES = ''''     /All sources selected')
 3001 FORMAT (A6,'/Sources excluded:')
 3002 FORMAT (A6,'/Sources included:')
 3003 FORMAT (A6,'SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'         ,''',A16,''',''',A16,'''')
 3005 FORMAT (A6,'ANTENNAS = 0     /All antennas selected')
 3006 FORMAT (A6,'/Antennas excluded:')
 3007 FORMAT (A6,'/Antennas included:')
 3008 FORMAT (A6,'ANTENNAS = ',12(I3,' '))
 3009 FORMAT (A6,'           ',12(I3,' '))
 3010 FORMAT (A6,'Smooth: Real and Imaginary')
 3015 FORMAT (A6,'Smooth: Amplitude and Phase')
 3016 FORMAT (A6,'Correcting amplitude only')
 3020 FORMAT (A6,'Type: ',A4,'; Width: ',F9.3,'; Support: ',
     *   F7.1,' chan')
 3021 FORMAT (A6,'Type: ',A4,'; spectral index ',F5.2)
 3022 FORMAT (A6,'Corrected from channels ',I5,' - ',I5)
 3030 FORMAT (A6,'Set phase to zero ?: ',A1,'; Ampl norml ?: ',A1)
      END
      SUBROUTINE NRMLBP (VIS, N, CHNS, VNULL)
C--------------------------------------------------------------------
C   Normalise the BP amplitude
C   Input parameters:
C      VIS      R(2,N)     Input complex bandpass (1=re,2=imag).
C      N        I          No of frequency channels in BP.
C      VNULL    R          Value of indeterminate pixel.
C   Ouput parameter:
C      VIS      R(2,N)     Normalized complex bandpass (1=re,2=imag).
C--------------------------------------------------------------------
      INTEGER   N
      REAL      VIS(2,N), CHNS(2), VNULL
C
      REAL      SUM, COUNT, FACT
      INTEGER   I, BC, EC
C----------------------------------------------------------------------
      BC = CHNS(1) + 0.5
      EC = CHNS(2) + 0.5
      IF (BC.LT.1) BC = N/8 + 1
      IF (EC.LT.BC) EC = N - N/8
C                                       Sum the BP amplitude
      SUM = 0.0
      COUNT = 0
      DO 100 I = BC,EC
         IF ((VIS(1,I).NE.VNULL) .AND. (VIS(2,I).NE.VNULL) .AND.
     *      (VIS(1,I).NE.0.0)) THEN
            SUM = SUM + SQRT (VIS(1,I)**2 + VIS(2,I)**2)
            COUNT = COUNT + 1
            END IF
 100     CONTINUE
C                                       Normalise
      FACT = 1.0
      IF (COUNT.GT.0) FACT = SUM / COUNT
      DO 200 I = 1,N
         IF ((VIS(1,I).NE.VNULL).AND.(VIS(2,I).NE.VNULL)) THEN
            VIS(1,I) = VIS(1,I) / FACT
            VIS(2,I) = VIS(2,I) / FACT
            END IF
 200     CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE SCONVL (LOPCOD, SDIAM, SUPORT, XDATA, XWORK,
     *   N, XNULL, IERR)
C----------------------------------------------------------------------
C   Smooth a complex array by convolving with a smoothing function.
C   Input parameters:
C      LOPCOD   C*4        Smoothing type
C                          'HANN'=Hanning; 'GAUS'=Gaussian,
C                          'SINC'=Sin x/x; 'BOXC'=Boxcar,
C                          'BMED'=Broadened median.
C      SDIAM    R          Diameter of smoothing func (channels).
C      SUPORT   R          Support of smoothing func (diam; chan).
C      XDATA    R(2,N)     Input complex array.
C      XWORK    R(2,N)     Work array.
C      N        I          No of points in XDATA.
C      XNULL    R          Value of the indeterminate pixel.
C   Output parameters:
C      XDATA    R(2,N)     Smoothed complex array.
C      IERR     I          Termination status (0=>ok).
C----------------------------------------------------------------------
      CHARACTER LOPCOD*4
      INTEGER   N, IERR
      REAL      XDATA(2,N), XWORK(2,N), SDIAM, SUPORT, XNULL
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MXTBF
      PARAMETER (MXTBF = 2*MAXCHA)
      REAL      TABFUN(MXTBF), FX, SUM, WGT, X, W
      INTEGER   I, J, K, M, J1, J2, NCOPY, NSAMPL, NMXTBF
      DATA NMXTBF /MXTBF/
C----------------------------------------------------------------------
      IERR = 0
C                                       Tabulate the smoothing function
C
C                                       Force suport to be symmetrical
C                                       about zero lag.
      NSAMPL = SUPORT + 0.1
      NSAMPL = NSAMPL - (1 - MOD (NSAMPL,2))
      NSAMPL = NSAMPL / 2
      NSAMPL = MIN (NSAMPL, NMXTBF)
      IF (NSAMPL.LT.0) NSAMPL = 0
      CALL RFILL (NSAMPL, 0.0, TABFUN)
      FX = 2.0 / SDIAM
C                                       Select the smoothing function.
      TABFUN(1) = 1.0
C                                       Hanning function
      IF (LOPCOD.NE.'HANN') GO TO 30
         DO 25 I = 2,NSAMPL
            X = I - 1
            TABFUN(I) = MAX (0.0, 1.0-FX*X)
  25        CONTINUE
C                                       Gaussian
  30  IF (LOPCOD.NE.'GAUS') GO TO 40
         FX = -LOG (2.0) * FX * FX
         DO 35 I = 2,NSAMPL
            X = I - 1
            TABFUN(I) = EXP (FX * X * X)
  35        CONTINUE
C                                       Boxcar
  40  IF (LOPCOD.NE.'BOXC') GO TO 50
         FX = 1.0 / FX
         DO 45 I = 2,NSAMPL
            X = I - 1
            IF (X.GT.FX) GO TO 50
               TABFUN(I) = 1.0
               IF (X.EQ.FX) TABFUN(I) = 0.5
  45        CONTINUE
C                                       Sinc
  50  IF (LOPCOD.NE.'SINC') GO TO 60
         FX = 3.14159265 * FX
         DO 55 I = 2,NSAMPL
            X = (I - 1) * FX
            TABFUN(I) = SIN (X) / X
  55        CONTINUE
C                                       Normalise the integral
  60  W = 1
      DO 120 I = 2,NSAMPL
         W = W + 2.0 * TABFUN(I)
 120     CONTINUE
      DO 130 I = 1,NSAMPL
         TABFUN(I) = TABFUN(I) / W
 130     CONTINUE
C                                       Copy XDATA to XWORK
      NCOPY = 2 * N
      CALL RCOPY (NCOPY, XDATA, XWORK)
C                                       Convolve XDATA with the
C                                       tabulated smoothing function.
      DO 300 I = 1,2
         DO 200 J = 1,N
            J1 = J - NSAMPL
            J2 = J + NSAMPL
            J1 = MAX (1, J1)
            J2 = MIN (N, J2)
            SUM = 0.0
            WGT = 0.0
            DO 180 K = J1,J2
               M = ABS (K-J) + 1.1
               IF (XWORK(I,K).EQ.XNULL) GO TO 180
                  SUM = SUM + XWORK(I,K) * TABFUN(M)
                  WGT = WGT + TABFUN(M)
 180           CONTINUE
            IF (XDATA(I,J).NE.XNULL) XDATA(I,J) = SUM / WGT
 200        CONTINUE
 300     CONTINUE
C                                       Exit
999   RETURN
      END
      SUBROUTINE MEDWIN (LOPCOD, NDIAM, XDATA, XWORK, XWORK2, N,
     *   XNULL, IERR)
C---------------------------------------------------------------------
C   Perform median window smoothing on a complex array.
C   Input paramters:
C      LOPCOD   C*4        Median filter type (BMED=Broadened median)
C      NDIAM    I          Diameter of median window (channels).
C      XDATA    R(2,N)     Input complex array.
C      XWORK    R(2,N)     Work array.
C      XWORK2   R(N)       Work array.
C      N        I          No of elements in XDATA.
C      XNULL    R          Value of indeterminate pixel.
C   Output parameter:
C      XDATA    R(2,N)     Smoothed complex array.
C      IERR     I          Termination status. 0=> ok.
C--------------------------------------------------------------------
      CHARACTER LOPCOD*4
      INTEGER NDIAM, N, IERR
      REAL XDATA(2,N), XWORK(2,N), XWORK2(N), XNULL, BMED
C
      REAL XVAL
      INTEGER I, J, K, M, J1, J2, NCOPY, NDIAM2
C---------------------------------------------------------------------
      IERR = 0
C                                       Force window width to
C                                       be symmetrical about
C                                       zero lag.
      NDIAM2 = (NDIAM - (1 - MOD (NDIAM,2))) / 2
C                                       Copy XDATA to XWORK
      NCOPY = 2 * N
      CALL RCOPY (NCOPY, XDATA, XWORK)
C                                       Loop through each array
C                                       and apply median window
C                                       smoothing.
      DO 300 I = 1,2
         DO 200 J = 1,N
C                                       Set window limits.
            J1 = J - NDIAM2
            J2 = J + NDIAM2
            J1 = MAX (1,J1)
            J2 = MIN (N,J2)
C                                       Copy window to XWRK2
            M = 0
            DO 180 K = J1,J2
               IF (XDATA(I,K).NE.XNULL) THEN
                  M = M + 1
                  XWORK2(M) = XWORK(I,K)
                  END IF
 180           CONTINUE
C                                       Select median filter type.
C                                       1: Broadened median filter
            IF (LOPCOD.EQ.'BMED') THEN
               XVAL = BMED (XWORK2, M, XNULL)
               END IF
C                                       Update XDATA
            IF (XDATA(I,J).NE.XNULL) XDATA(I,J) = XVAL
 200        CONTINUE
 300     CONTINUE
C                                       Exit
      RETURN
      END
      FUNCTION BMED (XDAT, N, XNULL)
C-------------------------------------------------------------------
C   Compute the broadened median of a one-dimensional array.
C   Ref: "Understanding Robust and Exploratory Data Analysis"
C   Hoaglin, Mosteller, Tukey:, p313.
C   Input parameters:
C      XDAT     R(*)       Input array.
C      N        I          Number of points in XDAT.
C      XNULL    R          Value of indeterminate pixel.
C-------------------------------------------------------------------
      REAL      BMED, XDAT(*), XNULL
      INTEGER   N
C
      REAL      XBMED, XTEMP
      INTEGER   JLIM1, JLIM2, J, I, K
C--------------------------------------------------------------------
      XBMED = XNULL
      IF (N.LE.0) GO TO 999
C                                       Derive the order statistics
C                                       by sorting XDAT in ascending
C                                       order.
      JLIM1 = N - 1
      DO 100 I = 1,JLIM1
         JLIM2 = I + 1
         DO 80 J = JLIM2,N
            IF (XDAT(I).GT.XDAT(J)) THEN
               XTEMP = XDAT(I)
               XDAT(I) = XDAT(J)
               XDAT(J) = XTEMP
               END IF
  80        CONTINUE
 100     CONTINUE
C                                       Is N odd or even ?
      K = MOD (N, 2)
      J = N / 2
C                                       N Odd:
      IF (K.NE.0) THEN
C                                       Case N of:
C                                       < 5:
         IF (N.LT.5) XBMED = XDAT (J+1)
C                                       5 <= N <= 12
         IF ((N.GE.5).AND.(N.LE.12))
     *      XBMED = (XDAT(J-1) + XDAT(J) + XDAT(J+1)) / 3.0
C                                       N > 12
         IF (N.GT.12)
     *      XBMED = (XDAT(J-1) + XDAT(J) + XDAT(J+1) + XDAT(J+2) +
     *         XDAT(J+3)) / 5.0
      ELSE
C                                       N Even:
C                                       1:
         IF (N.EQ.1) XBMED = XDAT(1)
C                                       2 <= N <= 4:
         IF ((N.GE.2).AND.(N.LE.4)) XBMED = (XDAT(J) + XDAT(J+1)) / 2.0
C                                       5 <= N <= 12:
         IF ((N.GE.5).AND.(N.LE.12)) XBMED = XDAT(J-1) / 6.0 +
     *      XDAT(J) / 3.0 + XDAT(J+1) / 3.0 + XDAT(J+2) / 6.0
C                                       N >= 13:
         IF (N.GT.12) XBMED = XDAT(J-2) / 10.0 + XDAT(J-1) / 5.0 +
     *      XDAT(J) / 5.0 + XDAT(J+1) / 5.0 + XDAT(J+2) / 5.0 +
     *      XDAT(J+3) / 10.0
         END IF
C                                       Exit
999   BMED = XBMED
      RETURN
      END
      SUBROUTINE BPCORF (OPER, NUMPOL, NUMIF, NUMFRQ, INFILE, BPFACT,
     *   XYBP, IERR)
C-----------------------------------------------------------------------
C   Reads in a text file of correction factors or applies them to the
C   data.
C   Inputs:
C      OPER     C*4    INIT or APPL
C      NUMPOL   I      # polarizations
C      NUMIF    I      # IFs
C      NUMFRQ   I      # spectral channels
C      INFILE   C*48   Text file name
C   In/out:
C      BPFACT   R(*)   Correction divisor
C      XYBP     R(*)   BP
C   Output:
C      IERR     I      Error code
C-----------------------------------------------------------------------
      CHARACTER OPER*(*), INFILE*(*)
      INTEGER   NUMPOL, NUMIF, NUMFRQ, IERR
      REAL      BPFACT(*), XYBP(2,NUMFRQ,NUMIF,NUMPOL)
C
      CHARACTER LINE*132
      INTEGER   IP, II, IC, LUN, FIND, KBP, JT, JTRIM
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       read it in
      IF (OPER.EQ.'INIT') THEN
         CALL RFILL (NUMFRQ, 1.0, BPFACT)
         LUN = 11
         CALL ZTXOPN ('READ', LUN, FIND, INFILE, .FALSE., IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN'
            GO TO 980
            END IF
         DO 50 II = 1,10000
            CALL ZTXIO ('READ', LUN, FIND, LINE, IERR)
            IF (IERR.EQ.2) GO TO 60
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'READ'
               GO TO 980
               END IF
C                                       check for comments
            JT = JTRIM (LINE)
            IF (LINE.EQ.' ') GO TO 50
            IF (LINE(:1).EQ.'$') GO TO 50
            IF (LINE(:1).EQ.'*') GO TO 50
            KBP = 1
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) GO TO 50
            IC = X + 0.01
            IF ((IC.LT.1) .OR. (IC.GT.NUMFRQ)) THEN
               WRITE (MSGTXT,1010) IC, NUMFRQ
               GO TO 980
               END IF
            CALL GETNUM (LINE, 132, KBP, X)
            IF (X.EQ.DBLANK) GO TO 50
            IF ((X.LE.1.E-4) .OR. (X.GT.1.E4)) THEN
               WRITE (MSGTXT,1015) X
               GO TO 980
               END IF
            BPFACT(IC) = X
 50         CONTINUE
 60      CALL ZTXCLS (LUN, FIND, IERR)
C                                       apply it
      ELSE
         IERR = 0
         DO 120 IP = 1,NUMPOL
            DO 110 II = 1,NUMIF
               DO 100 IC = 1,NUMFRQ
                  IF (XYBP(1,IC,II,IP).NE.FBLANK) THEN
                     XYBP(1,IC,II,IP) = XYBP(1,IC,II,IP) / BPFACT(IC)
                     XYBP(2,IC,II,IP) = XYBP(2,IC,II,IP) / BPFACT(IC)
                     END IF
 100              CONTINUE
 110           CONTINUE
 120        CONTINUE
         END IF
      GO TO 999
C
 980  CALL MSGWRT (6)
      IF (IERR.EQ.0) IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BPCORF: ERROR',I4,1X,A,'ING THE FUNC TEXT FILE')
 1010 FORMAT ('BPCORF CHANNEL',I5,' OUT OF RANGE 1 -',I5)
 1015 FORMAT ('BPCORF: CORR',1PE14.5,' UNREASONABLE')
      END

LOCAL INCLUDE 'SPCAL.INC'
C                                                         Include SPCAL
C                                       Local include for SPCAL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   MAXBUF, MAXCAL, MAXDAT
C                                       Size of local data buffer.
      PARAMETER (MAXBUF=5000000)
C                                       Max. no. visibilities
      PARAMETER (MAXDAT=10000)
C                                       Max. no. calibrators
      PARAMETER (MAXCAL=20)
C                                       Inputs adverbs
      REAL   XSI, XDI, XTIME(8), XBAND, XFREQ, XFQID, XBIF, XANTS(50),
     *   XUVRA(2), XSUBA, XBCHAN, XECHAN, XDOCAL, XGUSE, XBLVER, XFLAG,
     *   XDOBND, XBPVER, XSMOTH(3), XSOLIN, XPRTLV, XREF, XBADD(10),
     *   SOLINT
      HOLLERITH XNAMEI(3), XCLAIN(2), XXSOUR(4,30), XXSOLT
      CHARACTER NAMEIN*12, CLAIN*6, XSOUR(30)*16, XSOLTY*4
      INTEGER FREQID, NCHAN, IPRT, IAREF
      COMMON /XINPUT/ XNAMEI, XCLAIN, XSI, XDI, XXSOUR, XTIME, XBAND,
     *   XFREQ, XFQID, XBIF, XANTS, XUVRA, XSUBA, XBCHAN, XECHAN,
     *   XDOCAL, XGUSE, XBLVER, XFLAG, XDOBND, XBPVER, XSMOTH, XSOLIN,
     *   XXSOLT, XPRTLV, XREF, XBADD, SOLINT, IPRT, IAREF, FREQID, NCHAN
      COMMON /CHRCOM/ NAMEIN, CLAIN, XSOUR, XSOLTY
C                                       File/table buffers.
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   JBUFSZ, IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       Catalog header info.
      LOGICAL   SINGLE
      INTEGER   CATIN(256), DISKIN, SEQIN, CNOIN
      COMMON /CINFO/ SINGLE, CATIN, DISKIN, SEQIN, CNOIN
C                                       Local data buffer
      LOGICAL ISXY
      REAL TIME(MAXDAT), PARAN(2,MAXDAT), VISBUF(MAXBUF)
      INTEGER ANTS(2,MAXDAT), NUMDAT
      COMMON /VDATA/ TIME, PARAN, VISBUF, ISXY, NUMDAT, ANTS
C                                                          End SPCAL
LOCAL END
      PROGRAM SPCAL
C-----------------------------------------------------------------------
C! Determines Antenna polarization characteristics
C# UV Calibration AP-appl EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2000, 2003-2004, 2006-2007, 2009, 2015-2016
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
C   SPCAL computes feed polarization parameters for spectral line
C   UV-data and enters them in the AN table.
C   Input adverbs:
C      INNAME          Input UV file name (name)
C      INCLASS         Input UV file name (class)
C      INSEQ           Input UV file name (seq. #)
C      INDISK          Input UV file disk unit #
C                      Data selection (multisource):
C      CALSOUR         Sources to calibrate with
C      TIMERANG        Time range to use.
C      SELBAND         Bandwidth to select (kHz)
C      SELFREQ         Frequency to select (MHz)
C      FREQID          Freq. ID to select.
C      BIF             IF number.
C      ANTENNAS        Antennas to solve for.
C      UVRANGE         UV range in kilolamdba
C      SUBARRAY        Subarray, 0=>all
C      BCHAN           Lowest channel number 0=>all
C      ECHAN           Highest channel number 0=>all
C                      Cal. info for input:
C      FLAGVER         Flag table version
C      DOCALIB         If >0 calibrate data
C      GAINUSE         CAL table to apply.
C      SOLINT          Soln. interval (min) 0=>10.
C      PRTLEV          Print statistics 0=>none
C                      1 = some, 2 = lots. Use 1.
C      REFANT          Reference antenna, 0=none.
C      BADDISK         Disk no. not to use for scratch files.
C---------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'SPCAL.INC'
      DATA PRGM /'SPCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL SPCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Determine poln. parameters.
      CALL SPCUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       History
      CALL SPCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, IBUFF1)
C
 999  STOP
      END
      SUBROUTINE SPCIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   SPCIN gets input parameters for SPCAL and catalog header information.
C   Inputs:
C      PRGN    C*6     Program name
C   Output:
C      IRET    I         Error code: 0 => ok; <> 0 => error.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IERR, NPARM, I, NUMSUB, IROUND, LUN1, NUMAN(513)
      LOGICAL   T, F, TABLE, EXIST, FITASC, MATCH
      REAL      CATR(256), CATINR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'SPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DANT.INC'
      EQUIVALENCE (NUMAN, BUFF2)
      EQUIVALENCE (CATBLK, CATR, CATH, CATD)
      EQUIVALENCE (CATIN, CATINR)
      DATA LUN1 /28/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (T)
      CALL VHDRIN
      CALL SELINI
      JBUFSZ = 2 * UVBFSS
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      IRET = 0
C                                       Get input parameters.
      NPARM = 217
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, IBUFF1, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSI)
      DISKIN = IROUND (XDI)
      IPRT = XPRTLV + 0.1
      IPRT = MAX (IPRT, 0)
      IAREF = IROUND (XREF)
      SOLINT = XSOLIN / (24.0 * 60.0)
C                                       Default solution
C                                       interval of 10 min.
      IF (SOLINT.LE.0.0) SOLINT = 10.0 / (24.0 * 60.0)
C                                       Convert Hollerith strings.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      DO 20 I = 1,30
         CALL H2CHR (16, 1, XXSOUR(1,I), XSOUR(I))
 20      CONTINUE
      CALL H2CHR (4, 1, XXSOLT, XSOLTY)
      IF (XSOLTY.NE.'RAPR') XSOLTY = 'ZAPR'
C                                       Get CATBLK from uv file.
      CNOIN = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', IBUFF1, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1050) IRET
         GO TO 990
         END IF
C                                       Save input header.
      CALL COPY (256, CATBLK, CATUV)
      CALL COPY (256, CATBLK, CATIN)
C                                       See if a multiple source file
      LUNS(1) = 29
      CALL ISTAB ('SU', DISKIN, CNOIN, 1, LUNS, IBUFF1, TABLE, EXIST,
     *   FITASC, IERR)
C                                       Get uv header info.
      CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 999
      SINGLE = (.NOT.EXIST) .OR. (IERR.NE.0) .OR. (ILOCSU.LT.0)
C                                       Baddisk
      DO 70 I = 1,10
         IBAD(I) = IROUND (XBADD(I))
 70      CONTINUE
C                                       Linearly polarized feeds?
      ISXY = CATD(KDCRV+JLOCS) .LT. -4.0D0
C                                       Check sort order, must be T*
      IF (ISORT(1:1).NE.'T') THEN
         IRET = 4
         WRITE (MSGTXT,1070) ISORT, 'T*'
         GO TO 990
         END IF
C                                       Put selection criteria into
C                                       correct common.
      UNAME = NAMEIN
      UCLAS = CLAIN
      UDISK = DISKIN
      USEQ = SEQIN
      DO 80 I = 1,30
         SOURCS(I) = XSOUR(I)
         CALSOU(I) = ' '
 80      CONTINUE
      CALL RCOPY (8, XTIME, TIMRNG)
      DXTIME = 0.0
      UVRNG(1) = XUVRA(1)
      UVRNG(2) = XUVRA(2)
      STOKES = 'FULL'
      BCHAN = IROUND (XBCHAN)
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = IROUND (XECHAN)
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MAX (1, MIN (ECHAN, CATBLK(KINAX+JLOCF)))
      NCHAN = ECHAN - BCHAN + 1
      IF (JLOCIF.GE.0) THEN
         BIF = IROUND (XBIF)
         BIF = MAX (1, MIN (BIF, CATBLK(KINAX+JLOCIF)))
      ELSE
         BIF = 1
         END IF
      EIF = BIF
C                                       Freq id
      IF (XBAND.GT.0.0) SELBAN = XBAND
      IF (XFREQ.GT.0.0) SELFRQ = XFREQ
      FRQSEL = IROUND (XFQID)
      IF (FRQSEL.EQ.0) FRQSEL = -1
      CALL FQMATC (DISKIN, CNOIN, CATBLK, LUN1, SELBAN, SELFRQ,
     *   MATCH, FRQSEL, IRET)
      IF (.NOT.MATCH) THEN
         WRITE (MSGTXT,1060)
         IRET = 1
         GO TO 990
         END IF
      IF (IRET.GT.0) GO TO 999
      FREQID = FRQSEL
C                                       Antennas
      DO 85 I = 1,50
         ANTENS(I) = IROUND (XANTS(I))
 85      CONTINUE
      DOCAL = XDOCAL.GT.0.0
      DOWTCL = DOCAL .AND. (XDOCAL.LE.99.0)
      SUBARR = IROUND (XSUBA)
      IF (SUBARR.LE.0) SUBARR = 1
      DOBAND = IROUND (XDOBND)
      BPVER = IROUND (XBPVER)
      CALL RCOPY (3, XSMOTH, SMOOTH)
      BLVER = IROUND (XBLVER)
      FGVER = IROUND (XFLAG)
      CLVER = 0
      CLUSE = IROUND (XGUSE)
      DOPOL = -1
      DOSMTH = 0
      CALL RFILL (3, 0.0, SMOOTH)
      DOACOR = F
      DOFQSL = F
      INITVS = 1
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATBLK, NUMSUB)
      NUMSUB = MAX (1, NUMSUB)
      IF (NUMSUB.LT.SUBARR) THEN
         WRITE (MSGTXT,1160) SUBARR, NUMSUB
         GO TO 990
         END IF
C                                       Find number of antennas.
      CALL GETNAN (DISKIN, CNOIN, CATBLK, LUN1, IBUFF1, NUMAN, IRET)
      IF (IRET.NE.0) GO TO 999
      NUMANT = NUMAN(2)
      IF (SUBARR .LE. NUMAN(1)) NUMANT = NUMAN(1+SUBARR)
C                                       Number of IFs
      NUMIF = 1
      IF (JLOCIF.GE.0) NUMIF = CATBLK(KINAX+JLOCIF)
C                                       Number of polarizations
      NUMPOL = CATBLK(KINAX+JLOCS)
C                                       Check for full polzn.
      IF (NUMPOL.LT.4) THEN
         WRITE (MSGTXT,1170)
         IRET = 9
         GO TO 990
         ENDIF
C                                       Get antenna info
      CALL GETANT (DISKIN, CNOIN, SUBARR, CATBLK, IBUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Source selection.
      DOAPPL = F
      SELQUA = -1
      SELCOD = '    '
C      CALL SOUFIL (IRET)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SPCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,' . ',A6,' . ',
     *   I3,' DISK=',I3,' USID=',I4)
 1050 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1060 FORMAT ('NO MATCH TO SELBAND/SELFREQ ADVERBS - CHECK INPUTS')
 1070 FORMAT ('SORT ORDER IS ',A2,' NOT ',A2,' AS REQUIRED')
 1160 FORMAT ('SPECIFIED SUBARRAY ',I4,' > MAX. OF ',I4)
 1170 FORMAT ('UV-DATA FILE LACKS ALL FOUR POLZN. PAIRS')
      END
      SUBROUTINE SPCUV (IRET)
C-----------------------------------------------------------------------
C   SPCUV fits model parameters and enters them into the AN table.
C   Output:
C      IRET        I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      HOLLERITH CATINH(256)
      INTEGER   VERTMP, LUNNEW, LUNOLD, I, SOLTYP, NIF, MSGSAV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SPCAL.INC'
      COMPLEX DDD(2,MAXANT)
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATIN, CATINH)
      DATA LUNNEW, LUNOLD /27,28/
C-----------------------------------------------------------------------
C                                       Load calibrated data into
C                                       local buffer.
      CALL PCLSEL (IRET)
C                                       Check if any data:
      IF (IRET.NE.0) GO TO 999
C                                       Fit for feed parameters:
C                                       Similarity method
      IF (XSOLTY.EQ.'RAPR') THEN
C                                       Assumes linear polarization
C                                       is a scaled version of total
C                                       intensity in each channel.
         SOLTYP = 4
         CALL RPCALC (IPRT, IAREF, NUMANT, DDD, IRET)
      ELSE
C                                       Assumes circularly polarized
C                                       feeds and moderate linear
C                                       polarization.
         SOLTYP = 4
         CALL ZPCALC (IPRT, IAREF, NUMANT, DDD)
         END IF
      IF (IRET.NE.0) GO TO 999
      XREF = IAREF
C                                       Save feed parameters.
      DO 100 I = 1,NUMANT
         STNELP(1,BIF,I) = REAL (DDD(1,I))
         STNELP(2,BIF,I) = REAL (DDD(2,I))
         STNORI(1,BIF,I) = AIMAG (DDD(1,I))
         STNORI(2,BIF,I) = AIMAG (DDD(2,I))
 100     CONTINUE
C                                       Rewrite An table
C                                       Find number of AN tables:
      CALL FNDEXT ('AN', CATIN, VERTMP)
      IF (VERTMP.GT.0) VERTMP = VERTMP + 1
C                                       Copy to a temporary AN table.
      IF (JLOCIF.GE.0) THEN
         NIF = CATIN(KINAX+JLOCIF)
      ELSE
         NIF = 1
         END IF
      CALL PUTANT (DISKIN, CNOIN, SUBARR, VERTMP, BIF, EIF, NIF,
     *   SOLTYP, CATIN, IBUFF1, IBUFF2, FREQID, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Delete old
      CALL RMEXT (DISKIN, CNOIN, 'AN', SUBARR, CATIN, IBUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy back.
      MSGSAV = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('AN', VERTMP, SUBARR, LUNOLD, LUNNEW, DISKIN, DISKIN,
     *   CNOIN, CNOIN, CATIN, IBUFF1, IBUFF2, IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1610) IRET
         GO TO 990
         END IF
C                                       Delete temporary
      CALL RMEXT (DISKIN, CNOIN, 'AN', VERTMP, CATIN, IBUFF1, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
1610  FORMAT ('TABCOP ERROR ',I3,' UPDATING AN TABLE')
      END
      SUBROUTINE SPCHIS
C-----------------------------------------------------------------------
C   Routine to copy and update the history file.
C-----------------------------------------------------------------------
      CHARACTER CTIME(2)*12, HILINE*72
      INTEGER   LUN, IERR, I, ITIME(3), DATE(3)
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN /28/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Write history.
      CALL HIINIT (3)
C                                       Open old history.
      CALL HIOPEN (LUN, DISKIN, CNOIN, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Task message.
      CALL ZDATE (DATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, DATE, CTIME(2), CTIME)
      WRITE (HILINE,1000) TSKNAM, RLSNAM, CTIME
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Add selection/calibration
C                                       criteria:
C                                       Sources:
      IF (NSOUWD.LE.0) THEN
         WRITE (HILINE,3000) TSKNAM
         CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      ELSE
C                                       Included or excluded ?
         WRITE (HILINE,3001) TSKNAM
         IF (DOSWNT) WRITE (HILINE,3002) TSKNAM
         CALL HIADD (LUN, HILINE, IBUFF2, IERR)
         IF (IERR.NE.0) GO TO 190
C                                       1st and 2nd label.
         IF (.NOT.SINGLE) THEN
            WRITE (HILINE,3003) TSKNAM, XSOUR(1), XSOUR(2)
            CALL HIADD (LUN, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 190
C                                       Rest of sources.
            IF (NSOUWD.GT.2) THEN
               DO 20 I = 3,NSOUWD,2
                  WRITE (HILINE,3004) TSKNAM, XSOUR(I), XSOUR(I+1)
                  CALL HIADD (LUN, HILINE, IBUFF2, IERR)
                  IF (IERR.NE.0) GO TO 190
 20               CONTINUE
               END IF
            END IF
         END IF
C                                       Time range.
      CALL HITIME (TSTART, TEND, LUN, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Subarray.
      WRITE (HILINE,3013) TSKNAM, SUBARR
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Flag table.
      WRITE (HILINE,3014) TSKNAM, FGVER
      IF (DOFLAG) CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Calibration info.
C                                       Gain tables.
      IF (DOCAL) WRITE (HILINE,3019) TSKNAM, CLUSE
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Solution interval.
      WRITE (HILINE,2022) TSKNAM, XSOLIN
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Solution type.
      WRITE (HILINE,2023) TSKNAM, XSOLTY
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Reference antenna.
      I = XREF + 0.5
      WRITE (HILINE,2024) TSKNAM, I
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       IF's.
      WRITE (HILINE,2025) TSKNAM, BIF
      CALL HIADD (LUN, HILINE, IBUFF2, IERR)
      IF (IERR.NE.0) GO TO 190
C                                       Close history file.
 190   CALL HICLOS (LUN, T, IBUFF2, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (A6,'RELEASE =''',A7,' ''  /********* START ',
     *   A12,2X,A8)
 2022 FORMAT (A6,' SOLINT =',F10.3,' /SOLUTION INTERVAL (MIN)')
 2023 FORMAT (A6,' SOLTYPE =''',A4,''' /POLN. SOLUTION TYPE')
 2024 FORMAT (A6,' REFANT =',I4,' / REFERENCE ANTENNA')
 2025 FORMAT (A6,' BIF =',I3,' /IF NUMBER')
 3000 FORMAT (A6,' SOURCES = ''''     /ALL SOURCES SELECTED')
 3001 FORMAT (A6,' /SOURCES EXCLUDED:')
 3002 FORMAT (A6,' /SOURCES INCLUDED:')
 3003 FORMAT (A6,' SOURCES = ''',A16,''',''',A16,'''')
 3004 FORMAT (A6,'          ,''',A16,''',''',A16,'''')
 3013 FORMAT (A6,' SUBARRAY =',I4)
 3014 FORMAT (A6,' FLAGVER =',I3,' /FLAGGING TABLE USED')
 3019 FORMAT (A6,' GAINUSE = ',I3,' /CL TABLE APPLIED')
      END
      SUBROUTINE PCLSEL (IRET)
C--------------------------------------------------------------------
C   Reads a multi-source uv-data file into common arrays.
C   Output:
C      IRET        I    Termination status; 0=> ok
C--------------------------------------------------------------------
      INTEGER IRET
C
      LOGICAL WEOF, WGOOD, WPOLZN(1000), WSTOK(4)
      REAL  TIME2, RPARM(20), VIS(4096),
     *   CTIME, WT, SUMWT(1000), PANGLE(45)
      INTEGER NUMBL, NRECSZ, NLSOU, IA1, IA2,
     *   JNDX, JBASE, MNDX, JCHAN, JSTOK, KNDX,
     *   NBUFF, LUN1, NDAT, KNDX1, INDX, NFILL, NFILL2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SPCAL.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUN1 /27/
C--------------------------------------------------------------------
C                                       Pre-average time interval
      NUMBL = (NUMANT * (NUMANT - 1)) / 2
      NRECSZ = 12 * NCHAN
      TIME2 = -999.0
      NLSOU = -10
      NUMDAT = 0
C                                       Open data file.
      CALL UVGET ('INIT', RPARM, VIS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Initialise vis. buffer.
      NBUFF = NUMBL * NRECSZ
      CALL RFILL (NBUFF, 0.0, VISBUF)
C                                       Read UV record.
 50   CALL UVGET ('READ', RPARM, VIS, IRET)
      WEOF = IRET .EQ. -1
      IF ((IRET.LT.-1).OR.(IRET.GT.0)) GO TO 999
C                                       Extract UV parameters.
      CTIME = RPARM (ILOCT+1)
      IF (ILOCB.GE.0) THEN
         IA1 = RPARM(ILOCB+1) / 256.0 + 0.1
         IA2 = RPARM(ILOCB+1) - IA1*256 + 0.1
      ELSE
         IA1 = RPARM(ILOCA1+1) + 0.1
         IA2 = RPARM(ILOCA2+1) + 0.1
         END IF
      IF (ILOCSU.GE.0) CURSOU = RPARM(ILOCSU+1) + 0.1
C                                       Normalise
      IF ((CURSOU.NE.NLSOU).OR.(CTIME.GT.TIME2).OR.(WEOF)) THEN
         NDAT = NUMDAT
         DO 200 JBASE = 1,NUMBL
            WGOOD = .FALSE.
            MNDX = NDAT + JBASE
            DO 180 JCHAN = 1,NCHAN
               DO 160 JSTOK = 1,4
                  INDX = (MNDX-1)*NRECSZ+12*(JCHAN-1)+3*(JSTOK-1)+1
                  WT = VISBUF(INDX+2)
                  IF (WT.GT.0) THEN
                     VISBUF(INDX) = VISBUF(INDX) / WT
                     VISBUF(INDX+1) = VISBUF(INDX+1) / WT
                     WGOOD = .TRUE.
                  ELSE
                     VISBUF(INDX+2) = -1.0
                     ENDIF
 160              CONTINUE
 180           CONTINUE
C                                       Do not store null of
C                                       incomplete spectra
C                                       in the common arrays.
            IF (WGOOD.OR.WPOLZN(JBASE)) THEN
               NUMDAT = NUMDAT + 1
               KNDX = (NUMDAT - 1) * NRECSZ + 1
               KNDX1 = (MNDX - 1) * NRECSZ + 1
               CALL RCOPY (NRECSZ, VISBUF(KNDX1), VISBUF(KNDX))
               ANTS(1,NUMDAT) = ANTS(1,MNDX)
               ANTS(2,NUMDAT) = ANTS(2,MNDX)
               PARAN(1,NUMDAT) = PARAN(1,MNDX) / SUMWT(JBASE)
               PARAN(2,NUMDAT) = PARAN(2,MNDX) / SUMWT(JBASE)
               TIME(NUMDAT) = TIME(MNDX) / SUMWT(JBASE)
               ENDIF
 200        CONTINUE
C                                       Set accumulation limits.
         TIME2 = CTIME + SOLINT
C                                       Clear buffers.
         CALL RFILL (NUMBL, 0.0, SUMWT)
         JNDX = NUMDAT * NRECSZ + 1
         NBUFF = NUMBL * NRECSZ
C                                       Check array bounds
         IF ((JNDX+NBUFF-1).GT.MAXBUF) THEN
            IRET = 1
            WRITE (MSGTXT,1230)
            GO TO 990
            END IF
C
         CALL RFILL (NBUFF, 0.0, VISBUF(JNDX))
         DO 220 JBASE = 1,NUMBL
            WPOLZN(JBASE) = .FALSE.
 220        CONTINUE
         NFILL = MIN (NUMBL, MAXDAT - NUMDAT)
         NFILL2 = 2 * NFILL
         CALL RFILL (NFILL, 0.0, TIME(NUMDAT+1))
         CALL RFILL (NFILL2, 0.0, PARAN(1,NUMDAT+1))
         ENDIF
C                                       Accumulate the current record.
      IF (WEOF) GO TO 400
C                                       New Source ?
      IF (CURSOU.NE.NLSOU) THEN
         CALL GETSOU (CURSOU, DISKIN, CNOIN, CATUV, LUN1, IRET)
         IF (IRET.NE.0) GO TO 999
         NLSOU = CURSOU
         ENDIF
C                                       Compute the parallactic angle.
      CALL PARANG (CTIME, PANGLE)
      JBASE = (IA1 - 1) * NUMANT - (IA1 + 1) * IA1 / 2 + IA2
      MNDX = NUMDAT + JBASE
C                                       Check array bounds
      IF (MNDX.GT.MAXDAT) THEN
         IRET = 1
         WRITE (MSGTXT,1230)
         GO TO 990
         END IF
C
      ANTS(1,MNDX) = IA1
      ANTS(2,MNDX) = IA2
      DO 300 JCHAN = 1,NCHAN
         DO 280 JSTOK = 1,4
            WSTOK(JSTOK) = .FALSE.
            JNDX = (MNDX-1)*NRECSZ + 12*(JCHAN-1) + 3*(JSTOK-1) + 1
            INDX = 1 + (JSTOK - 1) * INCS + (JCHAN - 1) * INCF
            WT = VIS(INDX+2)
            IF (WT.GT.0) THEN
               WSTOK(JSTOK) = .TRUE.
               VISBUF(JNDX) = VISBUF(JNDX) + VIS(INDX) * WT
               VISBUF(JNDX+1) = VISBUF(JNDX+1) + VIS(INDX+1) * WT
               VISBUF(JNDX+2) = VISBUF(JNDX+2) + WT
               TIME(MNDX) = TIME(MNDX) + CTIME * WT
               PARAN(1,MNDX) = PARAN(1,MNDX) + PANGLE(IA1) * WT
               PARAN(2,MNDX) = PARAN(2,MNDX) + PANGLE(IA2) * WT
               SUMWT(JBASE) = SUMWT(JBASE) + WT
               ENDIF
 280        CONTINUE
         WPOLZN(JBASE) = WPOLZN(JBASE) .OR.
     *      (WSTOK(1).AND.WSTOK(2).AND.WSTOK(3)) .OR.
     *      (WSTOK(1).AND.WSTOK(2).AND.WSTOK(4))
 300     CONTINUE
C                                       Until EOF ?
 400  CONTINUE
      IF (.NOT.WEOF) GO TO 50
C                                       Close the UV data file.
      CALL UVGET ('CLOS', RPARM, VIS, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C--------------------------------------------------------------------
1230  FORMAT ('BUFFER OVERFLOW; INCREASE SOLUTION INTERVAL')
      END
      SUBROUTINE RPCALC (IPRTLV, IREF, N, D, IRET)
C--------------------------------------------------------------------
C   Calculate the antenna feed solutions.
C   Input parameter:
C      IPRTLV    I       Print level.
C      IREF      I       Reference antenna (0=> minimize rms
C                        antenna values.
C      N         I       Number of antennas.
C   Output parameter.
C      D         CX(2,*) R,L feed solutions for each antenna.
C      IRET      I       Termination status (0=> ok)
C-------------------------------------------------------------------
      INTEGER IREF, IRET, IPRTLV, N
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NVMAX, NVMAX1
      PARAMETER (NVMAX = 2*MAXANT + 128)
      PARAMETER (NVMAX1 = NVMAX + 1)
      INCLUDE 'SPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMPLEX AHA(NVMAX1,NVMAX1), AHB(NVMAX), WORK(NVMAX1),
     *   ZP1, ZP2, ZC(3), ZRR, ZLL, ZRL, ZLR, ZLRC, ZII,
     *   D(2,MAXANT), ZION1, ZION2
      DOUBLE PRECISION BHB, DPANG1, DPANG2, S, EPS,
     *   DXC(NVMAX), DRMS, DTIM
      REAL RTD, WTRL, WTLR, WTRR, WTLL, TEMP, SWT, RMS, AMP, AMPERR,
     *   PHASE, PHASER, E, FR1, FR2, DET(2), XRE, XIM, DOF, SWT2
      INTEGER LDA, NVAR, I, J, K, JCHAN, IA1, IA2, JNDX, I1, I2, I3,
     *   KNVAR, JOB, NVAR1, JPVT(NVMAX1), INFO, KOBS, NRECSZ, IER,
     *   COBS(MAXANT)
C-------------------------------------------------------------------
      IRET = 0
      MSGTXT = 'Determining feed solutions'
      CALL MSGWRT (6)
C                                       Set constants.
      NRECSZ = 12 * NCHAN
      RTD = 45.0 / ATAN (1.0)
      LDA = NVMAX1
      NVAR = 2 * N + NCHAN
C                                       Check array dimensions
      IF (NVAR.GT.NVMAX) THEN
         IRET = 1
         WRITE (MSGTXT,1020) NVAR, NVMAX
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Set up the normal equations.
      DO 20 I = 1,NVAR
         AHB(I) = (0.0, 0.0)
         DO 15 J = 1,NVAR
            AHA(I,J) = (0.0, 0.0)
 15         CONTINUE
 20      CONTINUE
      BHB = 0.0D0
      SWT = 0.0
      SWT2 = 0.0
      KOBS = 0
C                                       Loop over the observations.
      CALL FILL (MAXANT, 0, COBS)
      DO 100 K = 1,NUMDAT
         IA1 = ANTS(1,K)
         IA2 = ANTS(2,K)
         DO 80 JCHAN = 1,NCHAN
            JNDX = (K-1) * NRECSZ + 12*(JCHAN-1) + 1
C                                       Visiblities (RR,LL,RL,LR).
            ZRR = CMPLX (VISBUF(JNDX), VISBUF(JNDX+1))
            ZLL = CMPLX (VISBUF(JNDX+3), VISBUF(JNDX+4))
            ZRL = CMPLX (VISBUF(JNDX+6), VISBUF(JNDX+7))
            ZLR = CMPLX (VISBUF(JNDX+9), VISBUF(JNDX+10))
            WTRR = VISBUF(JNDX+2)
            WTLL = VISBUF(JNDX+5)
            WTRL = VISBUF(JNDX+8)
            WTLR = VISBUF(JNDX+11)
C                                       Form Stokes I.
C                                       Skip this channel if RR or
C                                       LL are missing.
            IF ((WTRR.LE.0.0).OR.(WTLL.LE.0.0)) GO TO 80
            ZII = (WTRR * ZRR + WTLL * ZLL) / (WTRR + WTLL)
C                                       Parallactic angles
            DPANG1 = 2.0D0 * PARAN(1,K)
            DPANG2 = 2.0D0 * PARAN(2,K)
            ZP1 = CMPLX (COS (DPANG1), SIN (DPANG1))
            ZP2 = CMPLX (COS (DPANG2), SIN (DPANG2))
            DTIM = DBLE (TIME(K))
C                                       No Faraday rotation at present.
            FR1 = 0.0
            FR2 = 0.0
            ZION1 = CMPLX (COS (FR1), SIN (FR1))
            ZION2 = CMPLX (COS (FR2), SIN (FR2))
C                                       RL exists ?
            IF (WTRL.GT.0) THEN
               COBS(IA1) = COBS(IA1) + 1
               COBS(IA2) = COBS(IA2) + 1
               I1 = IA1
               I2 = IA2 + N
               I3 = 2 * N + JCHAN
               SWT = SWT + WTRL
               SWT2 = SWT2 + WTRL*WTRL
               KOBS = KOBS + 1
               ZC(1) = ZLL * ZP1 * ZION1
               ZC(2) = ZRR * ZP2 * ZION2
               ZC(3) = ZII
               AHA(I1,I1) = AHA(I1,I1) + WTRL * CONJG(ZC(1)) * ZC(1)
               AHA(I1,I2) = AHA(I1,I2) + WTRL * CONJG(ZC(1)) * ZC(2)
               AHA(I1,I3) = AHA(I1,I3) + WTRL * CONJG(ZC(1)) * ZC(3)
               AHA(I2,I2) = AHA(I2,I2) + WTRL * CONJG(ZC(2)) * ZC(2)
               AHA(I2,I3) = AHA(I2,I3) + WTRL * CONJG(ZC(2)) * ZC(3)
               AHA(I3,I3) = AHA(I3,I3) + WTRL * CONJG(ZC(3)) * ZC(3)
               AHB(I1) = AHB(I1) + WTRL * CONJG(ZC(1)) * ZRL
               AHB(I2) = AHB(I2) + WTRL * CONJG(ZC(2)) * ZRL
               AHB(I3) = AHB(I3) + WTRL * CONJG(ZC(3)) * ZRL
               BHB = BHB + WTRL * (REAL(ZRL)**2 + AIMAG(ZRL)**2)
               ENDIF
C                                       LR exists ?
            IF (WTLR.GT.0) THEN
               COBS(IA1) = COBS(IA1) + 1
               COBS(IA2) = COBS(IA2) + 1
               I1 = IA2
               I2 = IA1 + N
               I3 = 2 * N + JCHAN
               SWT = SWT + WTLR
               SWT2 = SWT2 + WTLR*WTLR
               KOBS = KOBS + 1
               ZC(1) = CONJG (ZLL) * ZP2 * ZION2
               ZC(2) = CONJG (ZRR) * ZP1 * ZION1
               ZC(3) = CONJG (ZII)
               ZLRC = CONJG (ZLR)
               AHA(I1,I1) = AHA(I1,I1) + WTLR * CONJG(ZC(1)) * ZC(1)
               AHA(I1,I2) = AHA(I1,I2) + WTLR * CONJG(ZC(1)) * ZC(2)
               AHA(I1,I3) = AHA(I1,I3) + WTLR * CONJG(ZC(1)) * ZC(3)
               AHA(I2,I2) = AHA(I2,I2) + WTLR * CONJG(ZC(2)) * ZC(2)
               AHA(I2,I3) = AHA(I2,I3) + WTLR * CONJG(ZC(2)) * ZC(3)
               AHA(I3,I3) = AHA(I3,I3) + WTLR * CONJG(ZC(3)) * ZC(3)
               AHB(I1) = AHB(I1) + WTLR * CONJG(ZC(1)) * ZLRC
               AHB(I2) = AHB(I2) + WTLR * CONJG(ZC(2)) * ZLRC
               AHB(I3) = AHB(I3) + WTLR * CONJG(ZC(3)) * ZLRC
               BHB = BHB + WTLR * (REAL(ZLRC)**2 + AIMAG(ZLRC)**2)
               ENDIF
 80         CONTINUE
 100     CONTINUE
C                                       find ref ant
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         K = 0
         IREF = 0
         DO 105 I = 1,MAXANT
            IF (COBS(I).GT.K) THEN
               IREF = I
               K = COBS(I)
               END IF
 105        CONTINUE
         WRITE (MSGTXT,1105) IREF
         CALL MSGWRT (4)
         END IF
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      S = 0.0D0
      DO 110 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            S = S + REAL (AHA(I,I))
            END IF
 110     CONTINUE
      IF (K.GT.0) S = S / K
      IF (K.EQ.0) S = 1.0D0
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            TEMP = S
            AHA(I,I) = CMPLX (TEMP, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE
C                                       Either constrain the solution
C                                       for the right-hand i.f. of the
C                                       iref'th antenna to zero, or
C                                       modify the normal equations
C                                       matrix so as to approximate the
C                                       least-squares solution of
C                                       minimal Euclidean norm (by
C                                       adding a small positive number
C                                       to the diagonal elements):
      IF (IREF.GE.1.AND.IREF.LE.N) THEN
         AHA(IREF,IREF) = AHA(IREF,IREF) + 10.0 * S
      ELSE
         EPS = 0.0D0
         DO 180 I = 1,NVAR
            AHA(I,I) = (1.0D0 + EPS) * AHA(I,I)
180         CONTINUE
         END IF
C                                       Set up an extra column of AHA so
C                                       that the r.m.s. residual can
C                                       come as a by-product of the
C                                       Cholesky decomposition (see
C                                       p. 8-3 of the  LINPACK guide):
      DO 200 I = 1,NVAR
         AHA(I,NVAR+1) = AHB(I)
 200     CONTINUE
      AHA(NVAR+1,NVAR+1) = BHB
C                                       Get the Cholesky decomposition
C                                       of AHA via a LINPACK routine:
      JOB = 0
      NVAR1 = NVAR + 1
      CALL CCHDC (AHA, LDA, NVAR1, WORK, JPVT, JOB, INFO)
      IF (INFO.LT.NVAR) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (8)
         IER = 1
         DO 220 I = 1,N
            D(1,I) = (0.0 ,0.0)
            D(2,I) = (0.0 ,0.0)
 220        CONTINUE
         GO TO 999
         END IF
C                                       (The weighted sum of squared
C                                       residuals = real(AHA(nvar+1,
C                                       nvar+1))**2) Get the solution,
C                                       via a LINPACK routine:
      CALL CPOSL (AHA, LDA, NVAR, AHB)
C                                       Calculate the standard error
C                                       estimates, via a LINPACK routine
C                                       (the normalized covariance
C                                       matrix then is given by
C                                       AHA * RMS**2).
C                                       When the least-squares solution
C                                       of minimal Euclidean norm is
C                                       computed (i.e., when iref=0)
C                                       standard error estimates for
C                                       the antenna feed parameters are
C                                       not calculated here (they're
C                                       set to 0), but error estimates
C                                       for the calibrator fluxes can be
C                                       calculated:
      IF (KOBS.GT.KNVAR.AND.SWT.GT.0.) THEN
C         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
C     *      SQRT (REAL (KOBS)/(REAL (KOBS - KNVAR) * SWT * 2.0))
         DOF = SWT**2 / SWT2
         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
     *      SQRT (1./( (DOF - REAL(KNVAR)) * SWT * 2.0))
         WRITE (MSGTXT,1220) RMS, DOF
         CALL MSGWRT (4)
         JOB = 1
         CALL CPODI (AHA, LDA, NVAR, DET, JOB)
         IF (IREF.NE.0) AHA(IREF,IREF) = (0.0, 0.0)
         END IF
C                                       Now, print the results.
C                                       Then return:
      DO 320 I = 1,N
         D(1,I) = AHB(I)
         WRITE (MSGTXT,1300) I
         CALL MSGWRT (4)
         E = RMS * SQRT (REAL (AHA(I,I)) * SWT)
         AMP = ABS (D(1,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RTD
            PHASER = E / ABS (D(1,I)) * RTD
            END IF
         WRITE (MSGTXT,1301) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
C                                       Other hand poln.
         D(2,I) = CONJG (AHB(N+I))
         E = RMS * SQRT (REAL (AHA(N+I,N+I)) * SWT)
         AMP = ABS (D(2,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RTD
            PHASER = E / ABS (D(2,I)) * RTD
            END IF
         WRITE (MSGTXT,1302) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
 320     CONTINUE
C                                       Fitted polarization factors
      WRITE (MSGTXT,1320)
      CALL MSGWRT(4)
C
      DO 340 I = 1, NCHAN
         J = 2 * N + I
         E = RMS * SQRT (REAL (AHA(J,J)))
         XRE = REAL (AHB(J))
         XIM = AIMAG (AHB(J))
         WRITE (MSGTXT,1325) I, XRE, XIM, E
         CALL MSGWRT (4)
 340     CONTINUE
C                                       Compute and print residuals.
      IF (IPRTLV.GE.2) THEN
         DO 400 I = 1,N
            J = 4 * (I - 1) + 1
            DXC(J) = REAL (D(1,I))
            DXC(J+1) = AIMAG (D(1,I))
            DXC(J+2) = REAL (D(2,I))
            DXC(J+3) = AIMAG (D(2,I))
 400        CONTINUE
C
         CALL TRURMS (N, DXC, DRMS)
         WRITE (MSGTXT,1501) DRMS
         CALL MSGWRT (6)
         DO 420 J = 1,4*N
            DXC(J) = 0.0D0
 420        CONTINUE
         CALL TRURMS (N, DXC, DRMS)
         WRITE (MSGTXT,1502) DRMS
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('RPCALC: NVMAX TOO SMALL ',2I8)
 1105 FORMAT ('RPCAL: setting reference antenna to',I3)
 1200 FORMAT ('Solution is indeterminate.')
 1220 FORMAT ('RMS residual =',1PE12.3,' DOF =',0PF8.2)
 1300 FORMAT ('Interferometer Element',I4)
 1301 FORMAT ('  R: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1302 FORMAT ('  L: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1320 FORMAT ('Fitted polarization factors: solution type RAPR')
 1325 FORMAT ('Chan=',I4,' Q=',F11.5,' U=',F11.5,' (+/-',F11.5,')')
 1501 FORMAT ('True RMS residual =',1PD20.8)
 1502 FORMAT ('Pre-fit RMS =',1PD20.8)
      END
      SUBROUTINE ZPCALC (IPRTLV, IREF, N, D)
C-----------------------------------------------------------------------
C   Calculate the antenna feed solutions.
C   Input parameter:
C      IPRTLV    I       Print level.
C      IREF      I       Reference antenna (0=> minimize rms
C                        antenna values.
C      N         I       Number of antennas.
C   Output parameter.
C      D         CX(2,*) R,L feed solutions for each antenna.
C-----------------------------------------------------------------------
      INTEGER IREF, IPRTLV, N
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NVMAX, NVMAX1
      PARAMETER (NVMAX = 2*MAXANT)
      PARAMETER (NVMAX1 = 2*MAXANT + 1)
      INCLUDE 'SPCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMPLEX AHA(NVMAX1,NVMAX1), AHB(NVMAX), WORK(NVMAX1),
     *   ZP1, ZP2, ZC(2), ZRR, ZLL, ZRL, ZLR, ZLRC,
     *   D(2,MAXANT), ZION1, ZION2
      DOUBLE PRECISION BHB, DPANG1, DPANG2, S, EPS,
     *   DXC(NVMAX), DRMS, DTIM
      REAL      RTD, WTRL, WTLR, TEMP, SWT, RMS, AMP, AMPERR, DOF,
     *   PHASE, PHASER, E, FR1, FR2, DET(2), SWT2
      INTEGER   LDA, NVAR, I, J, K, JCHAN, IA1, IA2, JNDX, I1, I2,
     *   KNVAR, JOB, NVAR1, JPVT(NVMAX1), INFO, KOBS, NRECSZ, IER,
     *   COBS(MAXANT)
C-----------------------------------------------------------------------
      MSGTXT = 'Determining feed solutions'
      CALL MSGWRT (6)
C                                       Set constants.
      NRECSZ = 12 * NCHAN
      RTD = 45.0 / ATAN (1.0)
      LDA = NVMAX1
      NVAR = 2 * N
C                                       Set up the normal equations.
      DO 20 I = 1,NVAR
         AHB(I) = (0.0, 0.0)
         DO 15 J = 1,NVAR
            AHA(I,J) = (0.0, 0.0)
 15         CONTINUE
 20      CONTINUE
      BHB = 0.0D0
      SWT = 0.0
      SWT2 = 0.0
      KOBS = 0
C                                       Loop over the observations.
      CALL FILL (MAXANT, 0, COBS)
      DO 100 K = 1,NUMDAT
         IA1 = ANTS(1,K)
         IA2 = ANTS(2,K)
         DO 80 JCHAN = 1,NCHAN
            JNDX = (K-1) * NRECSZ + 12*(JCHAN-1) + 1
C                                       Visiblities (RR,LL,RL,LR).
            ZRR = CMPLX (VISBUF(JNDX), VISBUF(JNDX+1))
            ZLL = CMPLX (VISBUF(JNDX+3), VISBUF(JNDX+4))
            ZRL = CMPLX (VISBUF(JNDX+6), VISBUF(JNDX+7))
            ZLR = CMPLX (VISBUF(JNDX+9), VISBUF(JNDX+10))
            WTRL = VISBUF(JNDX+8)
            WTLR = VISBUF(JNDX+11)
            DPANG1 = 2.0D0 * PARAN(1,K)
            DPANG2 = 2.0D0 * PARAN(2,K)
            ZP1 = CMPLX (COS (DPANG1), SIN (DPANG1))
            ZP2 = CMPLX (COS (DPANG2), SIN (DPANG2))
            DTIM = DBLE (TIME(K))
C                                       No Faraday rotation at present.
            FR1 = 0.0
            FR2 = 0.0
            ZION1 = CMPLX (COS (FR1), SIN (FR1))
            ZION2 = CMPLX (COS (FR2), SIN (FR2))
C                                       RL exists ?
            IF (WTRL.GT.0) THEN
               COBS(IA1) = COBS(IA1) + 1
               COBS(IA2) = COBS(IA2) + 1
               I1 = IA1
               I2 = IA2 + N
               SWT = SWT + WTRL
               SWT2 = SWT2 + WTRL*WTRL
               KOBS = KOBS + 1
               ZC(1) = ZLL * ZP1 * ZION1
               ZC(2) = ZRR * ZP2 * ZION2
               AHA(I1,I1) = AHA(I1,I1) + WTRL * CONJG(ZC(1)) * ZC(1)
               AHA(I1,I2) = AHA(I1,I2) + WTRL * CONJG(ZC(1)) * ZC(2)
               AHA(I2,I2) = AHA(I2,I2) + WTRL * CONJG(ZC(2)) * ZC(2)
               AHB(I1) = AHB(I1) + WTRL * CONJG(ZC(1)) * ZRL
               AHB(I2) = AHB(I2) + WTRL * CONJG(ZC(2)) * ZRL
               BHB = BHB + WTRL * (REAL(ZRL)**2 + AIMAG(ZRL)**2)
               ENDIF
C                                       LR exists ?
            IF (WTLR.GT.0) THEN
               COBS(IA1) = COBS(IA1) + 1
               COBS(IA2) = COBS(IA2) + 1
               I1 = IA2
               I2 = IA1 + N
               SWT = SWT + WTLR
               SWT2 = SWT2 + WTLR*WTLR
               KOBS = KOBS + 1
               ZC(1) = CONJG (ZLL) * ZP2 * ZION2
               ZC(2) = CONJG (ZRR) * ZP1 * ZION1
               ZLRC = CONJG (ZLR)
               AHA(I1,I1) = AHA(I1,I1) + WTLR * CONJG(ZC(1)) * ZC(1)
               AHA(I1,I2) = AHA(I1,I2) + WTLR * CONJG(ZC(1)) * ZC(2)
               AHA(I2,I2) = AHA(I2,I2) + WTLR * CONJG(ZC(2)) * ZC(2)
               AHB(I1) = AHB(I1) + WTLR * CONJG(ZC(1)) * ZLRC
               AHB(I2) = AHB(I2) + WTLR * CONJG(ZC(2)) * ZLRC
               BHB = BHB + WTLR * (REAL(ZLRC)**2 + AIMAG(ZLRC)**2)
               ENDIF
 80         CONTINUE
 100     CONTINUE
C                                       find ref ant
      IF ((IREF.LE.0) .OR. (IREF.GT.N)) THEN
         K = 0
         IREF = 0
         DO 105 I = 1,MAXANT
            IF (COBS(I).GT.K) THEN
               IREF = I
               K = COBS(I)
               END IF
 105        CONTINUE
         WRITE (MSGTXT,1105) IREF
         CALL MSGWRT (4)
         END IF
C                                       Find the average size of the
C                                       nonzero diagonal elements of
C                                       AHA:
      K = 0
      S = 0.0D0
      DO 110 I = 1,NVAR
         IF (REAL (AHA(I,I)).GT.0.0) THEN
            K = K + 1
            S = S + REAL (AHA(I,I))
            END IF
 110     CONTINUE
      IF (K.GT.0) S = S / K
      IF (K.EQ.0) S = 1.0D0
C                                       For any missing antennas or
C                                       missing calibration sources,
C                                       set the corresponding diagonal
C                                       elements of the normal equations
C                                       matrix to the average of the
C                                       nonzero diagonal elements:
      KNVAR = NVAR
      DO 120 I = 1,NVAR
         IF (ABS (AHA(I,I)).EQ.0.0) THEN
            TEMP = S
            AHA(I,I) = CMPLX (TEMP, 0.0)
            KNVAR = KNVAR - 1
            END IF
 120     CONTINUE
C                                       Either constrain the solution
C                                       for the right-hand i.f. of the
C                                       iref'th antenna to zero, or
C                                       modify the normal equations
C                                       matrix so as to approximate the
C                                       least-squares solution of
C                                       minimal Euclidean norm (by
C                                       adding a small positive number
C                                       to the diagonal elements):
      IF (IREF.GE.1.AND.IREF.LE.N) THEN
         AHA(IREF,IREF) = AHA(IREF,IREF) + 10.0 * S
      ELSE
         EPS = 0.0D0
         DO 180 I = 1,NVAR
            AHA(I,I) = (1.0D0 + EPS) * AHA(I,I)
 180        CONTINUE
         END IF
C                                       Set up an extra column of AHA so
C                                       that the r.m.s. residual can
C                                       come as a by-product of the
C                                       Cholesky decomposition (see
C                                       p. 8-3 of the  LINPACK guide):
      DO 200 I = 1,NVAR
         AHA(I,NVAR+1) = AHB(I)
 200     CONTINUE
      AHA(NVAR+1,NVAR+1) = BHB
C                                       Get the Cholesky decomposition
C                                       of AHA via a LINPACK routine:
      JOB = 0
      NVAR1 = NVAR + 1
      CALL CCHDC (AHA, LDA, NVAR1, WORK, JPVT, JOB, INFO)
      IF (INFO.LT.NVAR) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (8)
         IER = 1
         DO 220 I = 1,N
            D(1,I) = (0.0 ,0.0)
            D(2,I) = (0.0 ,0.0)
 220        CONTINUE
         GO TO 999
         END IF
C                                       (The weighted sum of squared
C                                       residuals = real(AHA(nvar+1,
C                                       nvar+1))**2) Get the solution,
C                                       via a LINPACK routine:
      CALL CPOSL (AHA, LDA, NVAR, AHB)
C                                       Calculate the standard error
C                                       estimates, via a LINPACK routine
C                                       (the normalized covariance
C                                       matrix then is given by
C                                       AHA * RMS**2).
C                                       When the least-squares solution
C                                       of minimal Euclidean norm is
C                                       computed (i.e., when iref=0)
C                                       standard error estimates for
C                                       the antenna feed parameters are
C                                       not calculated here (they're
C                                       set to 0), but error estimates
C                                       for the calibrator fluxes can be
C                                       calculated:
      IF (KOBS.GT.KNVAR.AND.SWT.GT.0.) THEN
C         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
C     *      SQRT (REAL (KOBS)/(REAL (KOBS - KNVAR) * SWT * 2.0))
         DOF = SWT**2 / SWT2
         RMS = ABS (REAL (AHA(NVAR+1,NVAR+1))) *
     *      SQRT (1./( (DOF - REAL(KNVAR)) * SWT * 2.0))
         WRITE (MSGTXT,1220) RMS, DOF
          CALL MSGWRT (4)
         JOB = 1
         CALL CPODI (AHA, LDA, NVAR, DET, JOB)
         IF (IREF.NE.0) AHA(IREF,IREF) = (0.0, 0.0)
         END IF
C                                       Now, print the results.
C                                       Then return:
      DO 320 I = 1,N
         D(1,I) = AHB(I)
         WRITE (MSGTXT,1300) I
         CALL MSGWRT (4)
C                                       STM: AHA is now Inv(AHA) and
C                                       thus need to be scaled by SWT
         E = RMS * SQRT (REAL (AHA(I,I))*SWT)
         AMP = ABS (D(1,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(1,I)), REAL (D(1,I))) * RTD
            PHASER = E / ABS (D(1,I)) * RTD
         END IF
         WRITE (MSGTXT,1301) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
C                                       Other hand poln.
         D(2,I) = CONJG (AHB(N+I))
         E = RMS * SQRT (REAL (AHA(N+I,N+I))*SWT)
         AMP = ABS (D(2,I))
         AMPERR = SQRT (2.0) * E
         IF (AMP .EQ. 0.0) THEN
            PHASE = 0.0
            PHASER = 0.0
         ELSE
            PHASE = ATAN2 (AIMAG (D(2,I)), REAL (D(2,I))) * RTD
            PHASER = E / ABS (D(2,I)) * RTD
            END IF
         WRITE (MSGTXT,1302) AMP, AMPERR, PHASE, PHASER
         CALL MSGWRT (4)
 320     CONTINUE
C                                       Compute and print residuals.
      IF (IPRTLV.GE.2) THEN
         DO 400 I = 1,N
            J = 4 * (I - 1) + 1
            DXC(J) = REAL (D(1,I))
            DXC(J+1) = AIMAG (D(1,I))
            DXC(J+2) = REAL (D(2,I))
            DXC(J+3) = AIMAG (D(2,I))
 400        CONTINUE
         CALL TRURMS (N, DXC, DRMS)
         WRITE (MSGTXT,1501) DRMS
         CALL MSGWRT (6)
         DO 420 J = 1,4*N
            DXC(J) = 0.0D0
 420        CONTINUE
         CALL TRURMS (N, DXC, DRMS)
         WRITE (MSGTXT,1502) DRMS
         CALL MSGWRT (6)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1105 FORMAT ('ZPCAL: setting reference antenna to',I3)
 1200 FORMAT ('Solution is indeterminate.')
 1220 FORMAT ('RMS residual =',1PE12.3,' DOF =',0PF8.2)
 1300 FORMAT ('Interferometer Element',I4)
 1301 FORMAT ('  R: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1302 FORMAT ('  L: Amp = ',F8.5,'+/-',F8.5,' Phase(deg) = ',F7.2,
     *   '+/-',F7.2)
 1501 FORMAT ('True RMS residual =',1PD20.8)
 1502 FORMAT ('Pre-fit RMS =',1PD20.8)
      END
      SUBROUTINE TRURMS (N, DXC, DRMS)
C--------------------------------------------------------------------
C   Compute true rms for feed parameters.
C   Input:
C      N       I       No. of feed polzn. terms
C      DXC     CX(N)   Feed polarization terms.
C   Output:
C      DRMS    D       Output rms.
C--------------------------------------------------------------------
      INTEGER N
      DOUBLE PRECISION DXC(N), DRMS
C
      COMPLEX  ZDR1, ZDL1, ZDR2, ZDL2, ZRR, ZLL, ZRL, ZLR, ZDIFF,
     *   ZP1, ZP2, ZION1, ZION2
      DOUBLE PRECISION DCHI, DPANG1, DPANG2, DWTRL, DWTLR,
     *   DCOUNT, DFR1, DFR2, DTIM
      INTEGER J, JCHAN, JNDX, IA1, IA2, J1, J2, NRECSZ
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'SPCAL.INC'
C--------------------------------------------------------------------
      NRECSZ = 12 * NCHAN
      DCHI = 0.0D0
      DCOUNT = 0.0D0
C                                       Loop over all data points.
      DO 500 J = 1,NUMDAT
         IA1 = ANTS(1,J)
         IA2 = ANTS(2,J)
         J1 = (IA1 - 1) * 4
         ZDR1 = CMPLX (DXC(J1+1), DXC(J1+2))
         ZDL1 = CMPLX (DXC(J1+3), DXC(J1+4))
         J2 = (IA2 - 1) * 4
         ZDR2 = CMPLX (DXC(J2+1), DXC(J2+2))
         ZDL2 = CMPLX (DXC(J2+3), DXC(J2+4))
         DPANG1 = 2.0D0 * PARAN(1,J)
         DPANG2 = 2.0D0 * PARAN(2,J)
         ZP1 = CMPLX (COS(DPANG1), SIN(DPANG1))
         ZP2 = CMPLX (COS(DPANG2), SIN(DPANG2))
         DTIM = TIME(J)
C                                       No ionospheric Faraday
C                                       rotation at present.
         DFR1 = 0.0
         DFR2 = 0.0
         ZION1 = CMPLX (COS(DFR1), SIN(DFR1))
         ZION2 = CMPLX (COS(DFR2), SIN(DFR2))
C                                       Loop over each channel.
         DO 480 JCHAN = 1,NCHAN
            JNDX = (J-1) * NRECSZ + 12 * (JCHAN-1) + 1
            ZRR = CMPLX (VISBUF(JNDX), VISBUF(JNDX+1))
            ZLL = CMPLX (VISBUF(JNDX+3), VISBUF(JNDX+4))
            ZRL = CMPLX (VISBUF(JNDX+6), VISBUF(JNDX+7))
            ZLR = CMPLX (VISBUF(JNDX+9), VISBUF(JNDX+10))
C                                       RL exists ?
            DWTRL = VISBUF(JNDX+8)
            IF (DWTRL.GT.0) THEN
               ZDIFF = CONJG (ZDL2) * ZRR * ZP2 * ZION2 +
     *            ZDR1 * ZLL * ZP1 * ZION1 - ZRL
               DCHI = DCHI + (REAL (ZDIFF)) ** 2
               DCHI = DCHI + (AIMAG (ZDIFF)) ** 2
               DCOUNT = DCOUNT + 1.0D0
               ENDIF
C                                       LR exists ?
            DWTLR = VISBUF(JNDX+11)
            IF (DWTLR.GT.0) THEN
               ZDIFF = ZDL1 * ZRR * CONJG (ZP1*ZION1) +
     *            CONJG (ZDR2) * ZLL * CONJG (ZP2*ZION2) - ZLR
               DCHI = DCHI + (REAL (ZDIFF)) ** 2
               DCHI = DCHI + (AIMAG (ZDIFF)) ** 2
               DCOUNT = DCOUNT + 1.0D0
               ENDIF
480         CONTINUE
500      CONTINUE
C                                       Exit
      DRMS = SQRT (DCHI/DCOUNT)
      RETURN
      END
      SUBROUTINE PUTANT (DISK, CNO, INVER, OUTVER, BIF, EIF, NUMIF,
     *   SOLTYP, CATBLK, BUFF1, BUFF2, FREQID, IERR)
C-----------------------------------------------------------------------
C   PUTANT reads an antennas (AN) extension file and copies to an
C   output file adding antenna feed parameters.
C   Inputs:
C      DISK      I      Volume number
C      CNO       I      Catalog slot number
C      INVER     I      Input version number
C      OUTVER    I      Output version number
C      BIF       I      First IF number
C      EIF       I      Highest IF number
C      NUMIF     I      Number of IFs
C      SOLTYP    I      Feed solution type.
C                       1 = linear approximation
C                       2 = orientation, ellipticity
C                       3 = Lin. approx for X-Y feeds
C                       4 = VLBI linear approx.
C      CATBLK(*) I      Catalog header block
C      FREQID    I      FQ ID for which polzn parms being
C                       calculated
C   Input from COMMON (DANS.INC):
C      STNEPL(2,*)R    Feed real/elipticity (poln, IF)
C      STNORI(2,*)R    Feed imag/orientation (poln, IF)
C   Output:
C      BUFF1(*)  I      I/O Buffer
C      BUFF2(*)  I      I/O Buffer
C      IERR      I      Error code, 0=OK, else failed.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER CHPOLT*8, CHSOL(4)*8
      INTEGER   DISK, CNO, CATBLK(256), INVER, OUTVER, BUFF1(*),
     *   BUFF2(*), IERR,  BIF, EIF, NUMIF, SOLTYP, FREQID,
     *   A2NUMV(MAXANC), A2KOLS(MAXANC), IIF, LUN1, LUN2, IANT, INDEX,
     *   KEYTYP, LOCS, IBIF, IEIF, NUMREC, IATUTC
      HOLLERITH HSOLTY(2)
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA CHSOL /'APPROX  ', 'ORI-ELP ','X-Y LIN ', 'VLBI'/
      DATA CHPOLT /'POLTYPE '/
      DATA LUN1, LUN2 /27,28/
C-----------------------------------------------------------------------
C                                      Open AN extension file.
      CALL ANTINI ('READ', BUFF1, DISK, CNO, INVER, CATBLK, LUN1,
     *   IANRNO, ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'READ'
         GO TO 990
         END IF
C                                       Check FREQID compatibility.
      IF ((ANFQID.GT.0) .AND. (FREQID.GT.0) .AND.
     *   (ANFQID.NE.FREQID)) THEN
         WRITE (MSGTXT,1030)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1040)
         CALL MSGWRT (4)
         WRITE (MSGTXT,1050) ANFQID
         CALL MSGWRT (4)
         WRITE (MSGTXT,1060) FREQID
         CALL MSGWRT (4)
         IERR = 5
         GO TO 999
         END IF
C
      NUMREC = BUFF1(5)
C                                       Check IF limits
      IBIF = BIF
      IEIF = EIF
      IF (IBIF.LE.0) IBIF = 1
      IF (IBIF.GT.NUMIF) IBIF = NUMIF
      IF (IEIF.LE.IBIF) IEIF = IBIF
      IF (IEIF.GT.NUMIF) IEIF = NUMIF
C                                       Open output table
      NOPCAL = 2
      ANTNIF = NUMIF
      ANFQID = FREQID
      CALL ANTINI ('WRIT', BUFF2, DISK, CNO, OUTVER, CATBLK, LUN2,
     *   IANRNO, A2KOLS, A2NUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE,
     *   POLRXY, UT1UTC, IATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB,
     *   NOPCAL, ANTNIF, ANFQID, IERR)
      IF (IERR.GT.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         GO TO 990
         END IF
C                                       Read AN records
      DO 200 IANT = 1,NUMREC
         IANRNO = IANT
         CALL TABAN ('READ', BUFF1, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.LT.0) GO TO 200
         IF (IERR.GT.0) THEN
            WRITE (MSGTXT,1100) IERR, 'READ'
            GO TO 990
            END IF
C                                       Feed polarizations
         INDEX = 2 * (IBIF-1) + 1
         DO 150 IIF = IBIF,IEIF
            POLCA(INDEX) = STNELP(1,IIF,NOSTA)
            POLCA(INDEX+1) = STNORI(1,IIF,NOSTA)
            POLCB(INDEX) = STNELP(2,IIF,NOSTA)
            POLCB(INDEX+1) = STNORI(2,IIF,NOSTA)
            INDEX = INDEX + 2
 150        CONTINUE
C                                       Write record
         IANRNO = IANT
         CALL TABAN ('WRIT', BUFF2, IANRNO, A2KOLS, A2NUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'WRIT'
            GO TO 990
            END IF
 200     CONTINUE
C                                       Add solution type keyword.
      LOCS = 1
      KEYTYP = 3
      CALL CHR2H (8, CHSOL(SOLTYP), 1, HSOLTY)
      CALL TABKEY ('WRIT', CHPOLT, 1, BUFF2, LOCS, HSOLTY,
     *   KEYTYP, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1200) IERR
         GO TO 990
         END IF
C                                       Close AN extension files
      CALL TABIO ('CLOS', 1, IANRNO, BUFF1, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1210) IERR
         GO TO 990
         END IF
      CALL TABIO ('CLOS', 1, IANRNO, BUFF2, BUFF2, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1210) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PUTANT: ERROR',I3,' OPEN-FOR-',A4,'ING AN FILE')
 1030 FORMAT ('WARNING:')
 1040 FORMAT ('   The polarization information in your AN table')
 1050 FORMAT ('   was set with FREQID ',I3,' this is being')
 1060 FORMAT ('   overwritten with parameters derived from FREQID ',I3)
 1100 FORMAT ('PUTANT: ERROR',I3,1X,A4,'ING AN FILE')
 1200 FORMAT ('PUTANT: ERROR',I3,' ADDING SOLUTION TYPE KEYWORD')
 1210 FORMAT ('PUTANT: ERROR',I3,' CLOSING AN FILE')
      END
      SUBROUTINE CCHDC (A, LDA, P, WORK, JPVT, JOB, INFO)
C-----------------------------------------------------------------------
      INTEGER LDA,P,JPVT(1),JOB,INFO
      COMPLEX A(LDA,1),WORK(1)
C
C     CCHDC COMPUTES THE CHOLESKY DECOMPOSITION OF A POSITIVE DEFINITE
C     MATRIX.  A PIVOTING OPTION ALLOWS THE USER TO ESTIMATE THE
C     CONDITION OF A POSITIVE DEFINITE MATRIX OR DETERMINE THE RANK
C     OF A POSITIVE SEMIDEFINITE MATRIX.
C
C     ON ENTRY
C
C         A      COMPLEX(LDA,P).
C                A CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO
C                BE COMPUTED.  ONLT THE UPPER HALF OF A NEED BE STORED.
C                THE LOWER PART OF THE ARRAY A IS NOT REFERENCED.
C
C         LDA    INTEGER.
C                LDA IS THE LEADING DIMENSION OF THE ARRAY A.
C
C         P      INTEGER.
C                P IS THE ORDER OF THE MATRIX.
C
C         WORK   COMPLEX.
C                WORK IS A WORK ARRAY.
C
C         JPVT   INTEGER(P).
C                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
C                OF THE PIVOT ELEMENTS, IF PIVOTING HAS BEEN REQUESTED.
C                EACH DIAGONAL ELEMENT A(K,K)
C                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
C                VALUE OF JPVT(K).
C
C                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
C                                      ELEMENT.
C
C                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE ELEMENT.
C
C                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL ELEMENT.
C
C                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL ELEMENTS
C                ARE MOVED BY SYMMETRIC ROW AND COLUMN INTERCHANGES TO
C                THE BEGINNING OF THE ARRAY A AND FINAL
C                ELEMENTS TO THE END.  BOTH INITIAL AND FINAL ELEMENTS
C                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
C                FREE ELEMENTS ARE MOVED.  AT THE K-TH STAGE OF THE
C                REDUCTION, IF A(K,K) IS OCCUPIED BY A FREE ELEMENT
C                IT IS INTERCHANGED WITH THE LARGEST FREE ELEMENT
C                A(L,L) WITH L .GE. K.  JPVT IS NOT REFERENCED IF
C                JOB .EQ. 0.
C
C        JOB     INTEGER.
C                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
C                IF JOB .EQ. 0, NO PIVOTING IS DONE.
C                IF JOB .NE. 0, PIVOTING IS DONE.
C
C     ON RETURN
C
C         A      A CONTAINS IN ITS UPPER HALF THE CHOLESKY FACTOR
C                OF THE MATRIX A AS IT HAS BEEN PERMUTED BY PIVOTING.
C
C         JPVT   JPVT(J) CONTAINS THE INDEX OF THE DIAGONAL ELEMENT
C                OF A THAT WAS MOVED INTO THE J-TH POSITION,
C                PROVIDED PIVOTING WAS REQUESTED.
C
C         INFO   CONTAINS THE INDEX OF THE LAST POSITIVE DIAGONAL
C                ELEMENT OF THE CHOLESKY FACTOR.
C
C     FOR POSITIVE DEFINITE MATRICES INFO = P IS THE NORMAL RETURN.
C     FOR PIVOTING WITH POSITIVE SEMIDEFINITE MATRICES INFO WILL
C     IN GENERAL BE LESS THAN P.  HOWEVER, INFO MAY BE GREATER THAN
C     THE RANK OF A, SINCE ROUNDING ERROR CAN CAUSE AN OTHERWISE ZERO
C     ELEMENT TO BE POSITIVE. INDEFINITE SYSTEMS WILL ALWAYS CAUSE
C     INFO TO BE LESS THAN P.
C
C     LINPACK. THIS VERSION DATED 03/19/79 .
C     J.J. DONGARRA AND G.W. STEWART, ARGONNE NATIONAL LABORATORY AND
C     UNIVERSITY OF MARYLAND.
C
C
C     BLAS CAXPY,CSWAP
C     FORTRAN SQRT,REAL,CONJG
C
C     INTERNAL VARIABLES
C
      INTEGER PU,PL,PLP1,J,JP,JT,K,KB,KM1,KP1,L,MAXL
      INTEGER ITEMP, N1
      COMPLEX TEMP
      REAL MAXDIA
      LOGICAL SWAPK,NEGK
      DATA N1 /1/
C
      PL = 1
      PU = 0
      INFO = P
      IF (JOB .EQ. 0) GO TO 160
C
C        PIVOTING HAS BEEN REQUESTED. REARRANGE THE
C        THE ELEMENTS ACCORDING TO JPVT.
C
         DO 70 K = 1, P
            SWAPK = JPVT(K) .GT. 0
            NEGK = JPVT(K) .LT. 0
            JPVT(K) = K
            IF (NEGK) JPVT(K) = -JPVT(K)
            IF (.NOT.SWAPK) GO TO 60
               IF (K .EQ. PL) GO TO 50
                  ITEMP = PL-1
                  CALL CSWAP(ITEMP,A(1,K),N1,A(1,PL),N1)
                  TEMP = A(K,K)
                  A(K,K) = A(PL,PL)
                  A(PL,PL) = TEMP
                  A(PL,K) = CONJG(A(PL,K))
                  PLP1 = PL + 1
                  IF (P .LT. PLP1) GO TO 40
                  DO 30 J = PLP1, P
                     IF (J .GE. K) GO TO 10
                        TEMP = CONJG(A(PL,J))
                        A(PL,J) = CONJG(A(J,K))
                        A(J,K) = TEMP
                     GO TO 20
   10                CONTINUE
                     IF (J .EQ. K) GO TO 20
                        TEMP = A(K,J)
                        A(K,J) = A(PL,J)
                        A(PL,J) = TEMP
   20                CONTINUE
   30             CONTINUE
   40             CONTINUE
                  JPVT(K) = JPVT(PL)
                  JPVT(PL) = K
   50          CONTINUE
               PL = PL + 1
   60       CONTINUE
   70    CONTINUE
         PU = P
         IF (P .LT. PL) GO TO 150
         DO 140 KB = PL, P
            K = P - KB + PL
            IF (JPVT(K) .GE. 0) GO TO 130
               JPVT(K) = -JPVT(K)
               IF (PU .EQ. K) GO TO 120
                  ITEMP = K-1
                  CALL CSWAP(ITEMP,A(1,K),N1,A(1,PU),N1)
                  TEMP = A(K,K)
                  A(K,K) = A(PU,PU)
                  A(PU,PU) = TEMP
                  A(K,PU) = CONJG(A(K,PU))
                  KP1 = K + 1
                  IF (P .LT. KP1) GO TO 110
                  DO 100 J = KP1, P
                     IF (J .GE. PU) GO TO 80
                        TEMP = CONJG(A(K,J))
                        A(K,J) = CONJG(A(J,PU))
                        A(J,PU) = TEMP
                     GO TO 90
   80                CONTINUE
                     IF (J .EQ. PU) GO TO 90
                        TEMP = A(K,J)
                        A(K,J) = A(PU,J)
                        A(PU,J) = TEMP
   90                CONTINUE
  100             CONTINUE
  110             CONTINUE
                  JT = JPVT(K)
                  JPVT(K) = JPVT(PU)
                  JPVT(PU) = JT
  120          CONTINUE
               PU = PU - 1
  130       CONTINUE
  140    CONTINUE
  150    CONTINUE
  160 CONTINUE
      DO 270 K = 1, P
C
C        REDUCTION LOOP.
C
         MAXDIA = REAL(A(K,K))
         KP1 = K + 1
         MAXL = K
C
C        DETERMINE THE PIVOT ELEMENT.
C
         IF (K .LT. PL .OR. K .GE. PU) GO TO 190
            DO 180 L = KP1, PU
               IF (REAL(A(L,L)) .LE. MAXDIA) GO TO 170
                  MAXDIA = REAL(A(L,L))
                  MAXL = L
  170          CONTINUE
  180       CONTINUE
  190    CONTINUE
C
C        QUIT IF THE PIVOT ELEMENT IS NOT POSITIVE.
C
         IF (MAXDIA .GT. 0.0E0) GO TO 200
            INFO = K - 1
C     ......EXIT
            GO TO 280
  200    CONTINUE
         IF (K .EQ. MAXL) GO TO 210
C
C           START THE PIVOTING AND UPDATE JPVT.
C
            KM1 = K - 1
            CALL CSWAP(KM1,A(1,K),N1,A(1,MAXL),N1)
            A(MAXL,MAXL) = A(K,K)
            A(K,K) = CMPLX(MAXDIA, 0.0E0)
            JP = JPVT(MAXL)
            JPVT(MAXL) = JPVT(K)
            JPVT(K) = JP
            A(K,MAXL) = CONJG(A(K,MAXL))
  210    CONTINUE
C
C        REDUCTION STEP. PIVOTING IS CONTAINED ACROSS THE ROWS.
C
         WORK(K) = CMPLX(SQRT(REAL(A(K,K))),0.0E0)
         A(K,K) = WORK(K)
         IF (P .LT. KP1) GO TO 260
         DO 250 J = KP1, P
            IF (K .EQ. MAXL) GO TO 240
               IF (J .GE. MAXL) GO TO 220
                  TEMP = CONJG(A(K,J))
                  A(K,J) = CONJG(A(J,MAXL))
                  A(J,MAXL) = TEMP
               GO TO 230
  220          CONTINUE
               IF (J .EQ. MAXL) GO TO 230
                  TEMP = A(K,J)
                  A(K,J) = A(MAXL,J)
                  A(MAXL,J) = TEMP
  230          CONTINUE
  240       CONTINUE
            A(K,J) = A(K,J)/WORK(K)
            WORK(J) = CONJG(A(K,J))
            TEMP = -A(K,J)
            ITEMP = J-K
            CALL CAXPY(ITEMP,TEMP,WORK(KP1),N1,A(KP1,J),N1)
  250    CONTINUE
  260    CONTINUE
  270 CONTINUE
  280 CONTINUE
      RETURN
      END
      SUBROUTINE CPOSL(A,LDA,N,B)
      INTEGER LDA,N
      COMPLEX A(LDA,1),B(1)
C
C     CPOSL SOLVES THE COMPLEX HERMITIAN POSITIVE DEFINITE SYSTEM
C     A * X = B
C     USING THE FACTORS COMPUTED BY CPOCO OR CPOFA.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT FROM CPOCO OR CPOFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        B       COMPLEX(N)
C                THE RIGHT HAND SIDE VECTOR.
C
C     ON RETURN
C
C        B       THE SOLUTION VECTOR  X .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES
C        SINGULARITY BUT IT IS USUALLY CAUSED BY IMPROPER SUBROUTINE
C        ARGUMENTS.  IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED
C        CORRECTLY AND  INFO .EQ. 0 .
C
C     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
C     WITH  P  COLUMNS
C           CALL CPOCO(A,LDA,N,RCOND,Z,INFO)
C           IF (RCOND IS TOO SMALL .OR. INFO .NE. 0) GO TO ...
C           DO 10 J = 1, P
C              CALL CPOSL(A,LDA,N,C(1,J))
C        10 CONTINUE
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CAXPY,CDOTC
C
C     INTERNAL VARIABLES
C
      COMPLEX CDOTC,T
      INTEGER K,KB
      INTEGER ITEMP, N1
      DATA N1 /1/
C
C     SOLVE CTRANS(R)*Y = B
C
      DO 10 K = 1, N
         ITEMP = K-1
         T = CDOTC(ITEMP,A(1,K),N1,B(1),N1)
         B(K) = (B(K) - T)/A(K,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         ITEMP = K-1
         CALL CAXPY(ITEMP,T,A(1,K),N1,B(1),N1)
   20 CONTINUE
      RETURN
      END
      SUBROUTINE CPODI(A,LDA,N,DET,JOB)
      INTEGER LDA,N,JOB
      COMPLEX A(LDA,1)
      REAL DET(2)
C
C     CPODI COMPUTES THE DETERMINANT AND INVERSE OF A CERTAIN
C     COMPLEX HERMITIAN POSITIVE DEFINITE MATRIX (SEE BELOW)
C     USING THE FACTORS COMPUTED BY CPOCO, CPOFA OR CQRDC.
C
C     ON ENTRY
C
C        A       COMPLEX(LDA, N)
C                THE OUTPUT  A  FROM CPOCO OR CPOFA
C                OR THE OUTPUT  X  FROM CQRDC.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        A       IF CPOCO OR CPOFA WAS USED TO FACTOR  A  THEN
C                CPODI PRODUCES THE UPPER HALF OF INVERSE(A) .
C                IF CQRDC WAS USED TO DECOMPOSE  X  THEN
C                CPODI PRODUCES THE UPPER HALF OF INVERSE(CTRANS(X)*X)
C                WHERE CTRANS(X) IS THE CONJUGATE TRANSPOSE.
C                ELEMENTS OF  A  BELOW THE DIAGONAL ARE UNCHANGED.
C                IF THE UNITS DIGIT OF JOB IS ZERO,  A  IS UNCHANGED.
C
C        DET     REAL(2)
C                DETERMINANT OF  A  OR OF  CTRANS(X)*X  IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. DET(1) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF CPOCO OR CPOFA HAS SET INFO .EQ. 0 .
C
C     LINPACK.  THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS CAXPY,CSCAL
C     FORTRAN CONJG,MOD,REAL
C
C     INTERNAL VARIABLES
C
      COMPLEX T
      REAL S
      INTEGER J,JM1,K,KP1, I, ITEMP
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
         DET(1) = 1.0E0
         DET(2) = 0.0E0
         S = 10.0E0
         DO 50 I = 1, N
            DET(1) = REAL(A(I,I))**2*DET(1)
C        ...EXIT
            IF (DET(1) .EQ. 0.0E0) GO TO 60
   10       IF (DET(1) .GE. 1.0E0) GO TO 20
               DET(1) = S*DET(1)
               DET(2) = DET(2) - 1.0E0
            GO TO 10
   20       CONTINUE
   30       IF (DET(1) .LT. S) GO TO 40
               DET(1) = DET(1)/S
               DET(2) = DET(2) + 1.0E0
            GO TO 30
   40       CONTINUE
   50    CONTINUE
   60    CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(R)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 140
         DO 100 K = 1, N
            A(K,K) = (1.0E0,0.0E0)/A(K,K)
            T = -A(K,K)
            ITEMP = K-1
            CALL CSCAL(ITEMP,T,A(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) GO TO 90
            DO 80 J = KP1, N
               T = A(K,J)
               A(K,J) = (0.0E0,0.0E0)
               CALL CAXPY(K,T,A(1,K),1,A(1,J),1)
   80       CONTINUE
   90       CONTINUE
  100    CONTINUE
C
C        FORM  INVERSE(R) * CTRANS(INVERSE(R))
C
         DO 130 J = 1, N
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 120
            DO 110 K = 1, JM1
               T = CONJG(A(K,J))
               CALL CAXPY(K,T,A(1,J),1,A(1,K),1)
  110       CONTINUE
  120       CONTINUE
            T = CONJG(A(J,J))
            CALL CSCAL(J,T,A(1,J),1)
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
      COMPLEX FUNCTION CDOTC(N,CX,INCX,CY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS, CONJUGATING THE FIRST
C     VECTOR.
C     JACK DONGARRA, LINPACK,  3/11/78.
C
      COMPLEX CX(1),CY(1),CTEMP
      INTEGER I,INCX,INCY,IX,IY,N
C
      CTEMP = (0.0,0.0)
      CDOTC = (0.0,0.0)
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
        CTEMP = CTEMP + CONJG(CX(IX))*CY(IY)
        IX = IX + INCX
        IY = IY + INCY
   10 CONTINUE
      CDOTC = CTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
   20 DO 30 I = 1,N
        CTEMP = CTEMP + CONJG(CX(I))*CY(I)
   30 CONTINUE
      CDOTC = CTEMP
      RETURN
      END

LOCAL INCLUDE 'TLCAL.INC'
      INCLUDE 'INCS:PUVD.INC'
C
      CHARACTER NAMEIN*12, CLAIN*6, ASDMF(2)*64, SNAMES(1000)*16
      HOLLERITH XNAMEI(3), XCLAIN(2), XASDMF(16,2)
      REAL      XSIN, XDISIN, DOAPPL
      INTEGER   SEQIN, DISKIN, CNOIN, LUNF, INDF, NUMSOU, SCRTCH(512)
      REAL      SFLUXS(MAXIF,1000)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, DOAPPL, XASDMF
      COMMON /CHPARM/ SNAMES, NAMEIN, CLAIN, ASDMF
      COMMON /UVPCOM/ SCRTCH, SFLUXS, SEQIN, DISKIN, CNOIN, LUNF, INDF,
     *   NUMSOU
      INCLUDE 'INCS:DCHND.INC'
LOCAL END
      PROGRAM TLCAL
C-----------------------------------------------------------------------
C! TLCAL converts a JVLA telcal file to an SN table
C# Calibration VLA UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2018
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
C   TLCAL converts a telcal file to an SN table for JVLA
C   Inputs:
C      INNAME                             Input UV file name (name)
C      INCLASS                            Input UV file name (class)
C      INSEQ             0.0     9999.0   Input UV file name (seq. #)
C      INDISK                             Input UV file disk unit #
C      ASDMFILE                           File name in telcal area
C-----------------------------------------------------------------------
C
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'TLCAL.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'TLCAL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and create
C                                       output file if nec.
      CALL TLCLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 995
C                                       Write flag table
      CALL TLCLDO (IRET)
C                                       Close it down
 995  IRET = MAX (0, IRET)
      CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE TLCLIN (PRGM, IRET)
C-----------------------------------------------------------------------
C   TLCLIN gets input parameters for TLCAL .
C   Inputs:
C      PRGM   C*6   Program name
C   Output:
C      IRET   I     Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
C
      CHARACTER UTYPE*2, STAT*4, OBSDAT*8, ASDMFL*128
      INTEGER   IUSER, IERR, IROUND, NPARM, J, JTRIM, IDATE(3), TLUN, I,
     *   VER, K
      INCLUDE 'TLCAL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   SUKOLS(MAXSUC), SUNUMV(MAXSUC), ISURNO, SUNIF
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Init IO et al.
      TSKNAM = PRGM
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      CALL SELINI
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 40
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         IF (IERR.NE.1) THEN
            WRITE (MSGTXT,1000) IERR, 'OBTAINING INPUT PARAMETERS'
            CALL MSGWRT (8)
            END IF
         RQUICK = .FALSE.
         GO TO 999
         END IF
      IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Hollerith -> Char
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (64, 1, XASDMF(1,1), ASDMF(1))
      CALL H2CHR (64, 1, XASDMF(1,2), ASDMF(2))
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
C                                       Get CATBLK from file.
      IUSER = NLUSER
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, CNOIN, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   IUSER, STAT, SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      IUSER
         GO TO 990
         END IF
C                                       OK, get the header now
      CALL CATIO ('READ', DISKIN, CNOIN, CATBLK, 'REST', SCRTCH, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1035) IERR
         GO TO 990
         END IF
      CALL CHR2H (12, NAMEIN, 1, XNAMEI)
      CALL CHR2H (6, CLAIN, 1, XCLAIN)
      XDISIN = DISKIN
      XSIN = SEQIN
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = CNOIN
      FRW(NCFILE) = 0
C                                       duvh pointers
      CALL UVPGET (IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'SETTING UV HEADER POINTERS'
         GO TO 990
         END IF
C                                       open the gain file
      CALL H2CHR (8, 1, CATH(KHDOB), OBSDAT)
      CALL DATEST (OBSDAT, IDATE)
      WRITE (OBSDAT,1040) IDATE(1), IDATE(2)
      IF (ASDMF(2).EQ.' ') THEN
         J = JTRIM (ASDMF(1))
         ASDMFL = '/home/mchammer/evladata/telcal/' // OBSDAT //
     *      ASDMF(1)(:J) // '.GN'
      ELSE
         J = JTRIM (ASDMF(1))
         VER = JTRIM (ASDMF(2))
         ASDMFL = ASDMF(1)(:J) // ASDMF(2)(:VER) // '.GN'
         END IF
      LUNF = 3
      CALL ZTXOPN ('READ', LUNF, INDF, ASDMFL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'COULD NOT OPEN TELCAL FILE:'
         CALL MSGWRT (8)
         MSGTXT = ASDMFL(:80)
         GO TO 990
         END IF
C                                       Get frequencies
      VER = 1
      TLUN = 29
      CALL CHNDAT ('READ', SCRTCH, DISKIN, CNOIN, VER, CATBLK, TLUN,
     *   CHNIF, FOFF, ISBAND, FINC, BNDCOD, FQCHND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'READING FREQUENCY DATA'
         GO TO 990
         END IF
C                                       set frequencies
      DO 20 I = 1,CHNIF
         FOFF(I) = (FREQ + FOFF(I)) / 1.D6
 20      CONTINUE
C                                       Get source names
      DO 25 I = 1,1000
         SNAMES(I) = ' '
 25      CONTINUE
C                                       open table
      CALL SOUINI ('READ', SCRTCH, DISKIN, CNOIN, VER, CATBLK, TLUN,
     *   SUNIF, VELDEF, VELTYP, SUFQID, ISURNO, SUKOLS, SUNUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPENING THE SOURCE TABLE'
         GO TO 990
         END IF
      J = SCRTCH(5)
C                                       Read record
      NUMSOU = 0
      K = MAXIF * 1000
      CALL RFILL (K, 1.0, SFLUXS)
      DO 40 I = 1,J
         CALL TABSOU ('READ', SCRTCH, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *      SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *      EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *      PMDEC, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READING THE SOURCE TABLE'
            GO TO 990
            END IF
         SNAMES(IDSOUR) = SNAME
         NUMSOU = MAX (NUMSOU, IDSOUR)
         DO 30 K = 1,SUNIF
            IF (FLUX(1,K).LE.0.0) FLUX(1,K) = 1.0
            IF (FLUX(1,K).EQ.FBLANK) FLUX(1,K) = 1.0
            SFLUXS(K,IDSOUR) = SQRT (FLUX(1,K))
 30         CONTINUE
 40      CONTINUE
      CALL TABSOU ('CLOS', SCRTCH, ISURNO, SUKOLS, SUNUMV, IDSOUR,
     *   SNAME, QUAL, CALCOD, FLUX, FREQO, BANDW, RAEPO, DECEPO,
     *   EPOCH, RAAPP, DECAPP, RAOBS, DECOBS, LSRVEL, RESTFQ, PMRA,
     *   PMDEC, IRET)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TLCLIN: ERROR',I3,' ON ',A)
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK =',
     *   I3,' USER=',I5)
 1035 FORMAT ('ERROR',I3,' OBTAINING CATBLK ')
 1040 FORMAT (I4,'/',I2.2,'/')
      END
      SUBROUTINE TLCLDO (IRET)
C-----------------------------------------------------------------------
C   TLCLDO reads the telcal text file and writea an SN table
C   Outputs:
C      IRET     I   Error code
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   CLBUFF(512), SNVER, LUNS, ISNRNO, IANT, SNKOLS(MAXSNC),
     *   SNNUMV(MAXSNC), NUMANT, NUMPOL, NUMIF, NUMNOD, SOURID, ANTNO,
     *   SUBA, FREQID, NODENO, REFA(2,MAXIF), JTRIM, I, J, MJD0, NVAL,
     *   IPOL, IIF, NTXT, NTIM, NAV(28), NREC, NMSGFR, NIFAIL, NSFAIL,
     *   NMSGSU, NFLAGD, NCHRIF, JIF
      REAL      GMMOD, TIMEI, IFR, MBDELY(2), DISP(2), DDISP(2),
     *   CREAL(2,MAXIF,28), CIMAG(2,MAXIF,28), DELAY(2,MAXIF,28),
     *   RATE(2,MAXIF), WEIGHT(2,MAXIF,28), TEPS, AMP, PHS, DLY
      LOGICAL   ISAPPL, FINISH
      DOUBLE PRECISION  RANOD, DECNOD, TIME, DTEMP, SKYFRQ, MSGFRQ(1000)
      CHARACTER TXTLIN*256, IFCHAR*1, FLAGGD*5, SRC*16, LSOUR*16,
     *   SFAIL(100)*16, IFCHRS*5, CHARIF(MAXIF)*5
      INCLUDE 'TLCAL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      DATA LUNS /33/
      DATA IFR, MBDELY, DISP, DDISP /7*0.0/
      DATA SUBA, FREQID /2 * 1/
C-----------------------------------------------------------------------
      TEPS = 0.1 / (24.0 * 3600.0)
      I = 2 * MAXIF
      CALL RFILL (I, 0.0, RATE)
      NTXT = 0
      NTIM = 0
      NMSGFR = 0
      NIFAIL = 0
      NMSGSU = 0
      NSFAIL = 0
      NFLAGD = 0
      NCHRIF = 0
C                                       init the SN table
      NUMNOD = 0
      NODENO = 0
      NUMPOL = 1
      GMMOD = 1.0
      IF (NCOR.GT.1) NUMPOL = 2
      NUMIF = CHNIF
      ISAPPL = .FALSE.
      RANOD = 0.0D0
      DECNOD = 0.0D0
      SNVER = 0
      CALL SNINI ('WRIT', CLBUFF, DISKIN, CNOIN, SNVER, CATBLK, LUNS,
     *   ISNRNO, SNKOLS, SNNUMV, NUMANT, NUMPOL, NUMIF, NUMNOD, GMMOD,
     *   RANOD, DECNOD, ISAPPL, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'CREATING NEW SN TABLE'
         GO TO 990
         END IF
C                                       skip first 3 lines
      DO 10 I = 1,3
         CALL ZTXIO ('READ', LUNF, INDF, TXTLIN, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) IRET, 'INITIAL READING OF TEXT FILE'
            GO TO 990
            END IF
 10      CONTINUE
      NVAL = 0
      CALL FILL (28, 0, NAV)
      TIME = -10.0D0
      MJD0 = 0
C                                       read loop
 20   CALL ZTXIO ('READ', LUNF, INDF, TXTLIN, IRET)
      FINISH = IRET.EQ.2
      IF ((IRET.NE.0) .AND. (IRET.NE.2)) THEN
         WRITE (MSGTXT,1000) IRET, 'READING OF TEXT FILE'
         GO TO 990
         END IF
      IF (.NOT.FINISH) THEN
         J = JTRIM (TXTLIN)
         IF (MJD0.LE.0) READ (TXTLIN,1020) MJD0
         READ (TXTLIN,1021) DTEMP
         DTEMP = DTEMP - MJD0
         NTXT = NTXT + 1
      ELSE
         DTEMP = 1000.
         END IF
C                                       new time
      IF (ABS(DTEMP-TIME).GT.TEPS) THEN
C                                       write out current values
         IF (NVAL.GT.0) THEN
            TIMEI = DTEMP - TIME
            NTIM = NTIM + 1
            DO 30 I = 1,28
               ANTNO = I
               IF (NAV(I).GT.0) THEN
                  CALL TABSN ('WRIT', CLBUFF, ISNRNO, SNKOLS, SNNUMV,
     *               NUMPOL, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *               IFR, NODENO, MBDELY, DISP, DDISP, CREAL(1,1,I),
     *               CIMAG(1,1,I), DELAY(1,1,I), RATE, WEIGHT(1,1,I),
     *               REFA, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRITING SN TABLE'
                     GO TO 990
                     END IF
                  NREC = NREC + 1
                  END IF
 30            CONTINUE
            END IF
         IF (FINISH) GO TO 900
C                                       init
         NVAL = 0
         I = 2 * MAXIF * 28
         CALL RFILL (I, 0.0, WEIGHT)
         CALL RFILL (I, FBLANK, CREAL)
         CALL RFILL (I, FBLANK, CIMAG)
         CALL RFILL (I, FBLANK, DELAY)
         CALL FILL (28, 0, NAV)
         LSOUR = ' '
         END IF
C                                       parse line
      READ (TXTLIN,1030,ERR=890) IFCHRS, SKYFRQ, IANT, AMP, PHS, DLY,
     *   FLAGGD, SRC
      IFCHAR = IFCHRS(:1)
      IF (IFCHAR.EQ.'C') IFCHRS(:1) = 'A'
      IF (IFCHAR.EQ.'D') IFCHRS(:1) = 'B'
C                                       flagged
      NAV(IANT) = NAV(IANT) + 1
      IF (FLAGGD.NE.'false') THEN
         NFLAGD = NFLAGD + 1
C                                       not flagged
      ELSE
         JIF = 0
         DO 35 I = 1,NCHRIF
            IF (IFCHRS.EQ.CHARIF(I)) JIF = I
 35         CONTINUE
         IF (JIF.LE.0) THEN
            NCHRIF = NCHRIF + 1
            CHARIF(NCHRIF) = IFCHRS
            JIF = NCHRIF
            END IF
C                                       init source number
         IF (LSOUR.EQ.' ') THEN
            SOURID = 0
            DO 50 I = 1,NUMSOU
               IF (SNAMES(I).EQ.SRC) SOURID = I
 50            CONTINUE
            IF (SOURID.LE.0) THEN
               NSFAIL = NSFAIL + 1
               DO 55 I = 1,NMSGSU
                  IF (SRC.EQ.SFAIL(I)) GO TO 20
 55               CONTINUE
               WRITE (MSGTXT,1040) SRC
               CALL MSGWRT (7)
               NMSGSU = NMSGSU + 1
               SFAIL(NMSGSU) = SRC
               GO TO 20
               END IF
            LSOUR = SRC
            END IF
C                                       check source
         IF (LSOUR.NE.SRC) THEN
            WRITE (MSGTXT,1041) LSOUR, SRC
            CALL MSGWRT (7)
            END IF
C                                       polarization
         IPOL = 1
         IF ((IFCHAR.EQ.'C') .OR. (IFCHAR.EQ.'D')) IPOL = 2
         IIF = 0
         IF (ABS(FOFF(JIF)-SKYFRQ).LT.0.1) IIF = JIF
         IF (IIF.LE.0) THEN
            DO 60 I = 1,NUMIF
               IF (ABS(FOFF(I)-SKYFRQ).LT.0.1) IIF = I
 60            CONTINUE
            END IF
C                                       limit failure messages
         IF (IIF.LE.0) THEN
            NIFAIL = NIFAIL + 1
            DO 65 I = 1,NMSGFR
               IF (ABS(SKYFRQ-MSGFRQ(I)).LT.0.1) GO TO 20
 65            CONTINUE
            WRITE (MSGTXT,1050) SKYFRQ
            CALL MSGWRT (7)
            NMSGFR = NMSGFR + 1
            MSGFRQ(NMSGFR) = SKYFRQ
C                                       found the IF
         ELSE
            NVAL = NVAL + 1
            TIME = DTEMP
            IF (AMP.NE.0.0) AMP = 1.0 / AMP
            IF ((DOAPPL.GT.0.0) .AND. (SOURID.GT.0)) AMP = AMP *
     *         SFLUXS(IIF,SOURID)
            CREAL(IPOL,IIF,IANT) = AMP * COS (DG2RAD*PHS)
            CIMAG(IPOL,IIF,IANT) = AMP * SIN (DG2RAD*PHS)
            DELAY(IPOL,IIF,IANT) = DLY*1.D-9
            WEIGHT(IPOL,IIF,IANT) = 1.0
            END IF
         END IF
      GO TO 20
C                                       read error
 890  MSGTXT = 'PARSE LINE FORMAT ERROR'
      CALL MSGWRT (8)
C                                       normal finish
 900  CALL ZTXCLS (LUNF, INDF, I)
      I = 1
      CALL TABSN ('CLOS', CLBUFF, ISNRNO, SNKOLS, SNNUMV, NUMPOL, TIME,
     *   TIMEI, SOURID, ANTNO, SUBA, FREQID, IFR, NODENO, MBDELY, DISP,
     *   DDISP, CREAL(1,1,I), CIMAG(1,1,I), DELAY(1,1,I), RATE,
     *   WEIGHT(1,1,I), REFA, IANT)
      WRITE (MSGTXT,1900) NTXT
      CALL MSGWRT (3)
      WRITE (MSGTXT,1901) NTIM, NREC
      CALL MSGWRT (3)
      WRITE (MSGTXT,1902) SNVER
      CALL MSGWRT (3)
      IF (NSFAIL.GT.0) THEN
         WRITE (MSGTXT,1903) NSFAIL
         CALL MSGWRT (7)
         END IF
      IF (NIFAIL.GT.0) THEN
         WRITE (MSGTXT,1904) NIFAIL
         CALL MSGWRT (7)
         END IF
      IF (NFLAGD.GT.0) THEN
         WRITE (MSGTXT,1905) NFLAGD
         CALL MSGWRT (7)
         END IF
      GO TO 999
C                                       error finish
 990  CALL MSGWRT (8)
      GO TO 900
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TLCLDO ERROR',I4,' ON ',A)
 1020 FORMAT (I5)
 1021 FORMAT (F18.12)
 1030 FORMAT (58X,A5,1X,F9.2,4X,I2,F8.4,F8.1,10X,F11.4,5X,A5,37X,A)
 1040 FORMAT ('SOURCE NAME NOT FOUND IN SU TABLE = ''',A,'''')
 1041 FORMAT ('EXPECT SOURCE ',A,' FOUND ',A)
 1050 FORMAT ('SKYFRQ',F10.2,' NOT RECOGNIZED')
 1900 FORMAT ('Read',I8,' text records')
 1901 FORMAT ('Wrote',I7,' times and',I8,' total records')
 1902 FORMAT ('To SN table version',I4)
 1903 FORMAT (I10,' INPUT RECORDS DID NOT HAVE A MATCHING SOURCE')
 1904 FORMAT (I10,' INPUT RECORDS DID NOT HAVE A MATCHING FREQUENCY')
 1905 FORMAT (I10,' INPUT RECORDS WERE MARKED AS FLAGGED')
      END

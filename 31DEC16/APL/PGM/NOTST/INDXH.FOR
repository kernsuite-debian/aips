LOCAL INCLUDE 'INDXH.INC'
C                                       Local include for INDXH
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, DISKIN, JBUFSZ, OLDCNO, NUMAN(513), NUMANT
      HOLLERITH XNAMEI(3), XCLAIN(2)
      CHARACTER NAMEIN*12, CLAIN*6
      REAL      XSIN, XDISIN, CPARM(10), BUFF1(UVBFSS)
      LOGICAL   NEWCL, NEWCS, REDEL
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, CPARM,
     *   SEQIN, DISKIN, OLDCNO, NUMAN, NUMANT, NEWCL, NEWCS, REDEL
      COMMON /CHRCOM/ NAMEIN, CLAIN
LOCAL END
      PROGRAM INDXH
C-----------------------------------------------------------------------
C! Indexes a multisource uv data file.
C# UV Calibration EXT-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000-2001, 2007-2008, 2011, 2015-2016
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
C   Index a 'T*' order multi source data file.
C   Inputs:
C      AIPS adverb          Description.
C      INNAME.....Input UV file name (name).      Standard defaults.
C      INCLASS....Input UV file name (class).     Standard defaults.
C      INSEQ......Input UV file name (seq. #).    0 => highest.
C      INDISK.....Disk drive # of input UV file.  0 => any.
C      CPARM......1=max. gap, 2=max scan, 3=CL/CS entry interval,
C                 4=recalc CL group delays from IM table (VLBA only).
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'INDXH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHIS.INC'
      DATA PRGM /'INDXH '/
C-----------------------------------------------------------------------
C                                       Get input parameters.
      CALL NDXRIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Index file.
      CALL NDXRUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE NDXRIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   NDXRIN gets input parameters for INDXH and finds input file.
C   Inputs:  PRGN    C*6      Program name
C   Output:  JERR    I    Error code: 0 => ok
C                                3 => Wrong sort order
C                                4 => No source table
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in INDXH for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER   JERR
C
      CHARACTER STAT*4, UTYPE*2
      INTEGER   NPARM, IROUND, IERR, CLUN, ALUN, ISUB, NSUBA, NIM
      LOGICAL   TABLE, EXIST, FITASC
      LOGICAL   T
      INCLUDE 'INDXH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA CLUN, ALUN /28, 29/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 17
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
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
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
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'WRIT', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Check sort order
      IF (ISORT(:1).NE.'T') THEN
         JERR = 3
         WRITE (MSGTXT,1050) ISORT
         GO TO 990
         END IF
      UTYPE = 'CL'
      IF (TYPUVD.GT.0) UTYPE = 'CS'
      CALL FXHDEX (CATBLK)
C                                       See if CL table #1 around
      CALL ISTAB (UTYPE, DISKIN, OLDCNO, 1, CLUN, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      NEWCL = (.NOT.EXIST) .AND. (TYPUVD.LE.0)
      NEWCS = (.NOT.EXIST) .AND. (TYPUVD.GT.0)
      IF (CPARM(3).LT.0.0) THEN
         NEWCL = .FALSE.
         NEWCS = .FALSE.
         END IF
C                                       Warn user if CL table requested
C                                       but not written
      IF ((EXIST) .AND. ((CPARM(3).GT.0.0) .OR. (CPARM(4).GT.0.0))) THEN
         MSGTXT = UTYPE // ' #1 EXISTS; CPARM(3) AND CPARM(4) IGNORED'
         CALL MSGWRT (6)
         END IF
C                                       Get number of antennas
      IF ((NEWCL) .OR. (NEWCS)) THEN
         CALL GETNAN (DISKIN, OLDCNO, CATBLK, ALUN, BUFF1, NUMAN, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1070) JERR
            CALL MSGWRT (7)
            NEWCL = .FALSE.
            IF ((TYPUVD.GT.0) .AND. (CPARM(5).GE.0.9)) THEN
               NSUBA = 1
               NUMAN(1) = 1
               NUMAN(2) = CPARM(5) + 0.5
               NUMANT = NUMAN(2)
               JERR = 0
            ELSE
               NEWCS = .FALSE.
               END IF
         ELSE
            NSUBA = NUMAN(1)
            NUMANT = 0
            DO 100 ISUB = 1,NSUBA
               NUMANT = MAX (NUMANT, NUMAN(ISUB+1))
 100           CONTINUE
            END IF
         END IF
C                                       Default CPARMs
C                                       Longest gap.
      IF (CPARM(1).LE.1.0E-25) CPARM(1) = 10.0
C                                       Longest scan.
      IF (CPARM(2).LE.1.0E-25) CPARM(2) = 60.0
C                                       CL interval
      IF (CPARM(3).LE.1.0E-25) CPARM(3) = 5.0
C                                       VLBA only: Recalc group delays from
C                                       IM table?
      REDEL = (TYPUVD.LE.0) .AND. (CPARM(4).GE.1.0E-25)
      IF (REDEL) THEN
         CALL FNDEXT ('IM', CATBLK, NIM)
         IF (NIM.EQ.0) THEN
            MSGTXT = 'CPARM(4) > 0, but no IM tables present.'
            CALL MSGWRT (6)
            MSGTXT = 'Will carry on but not update delay in CL table'
            CALL MSGWRT (6)
            REDEL = .FALSE.
            END IF
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NDXRIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('WRONG SORT ORDER(',A2,'), USE UVSRT TO SORT TO ''TB''')
 1070 FORMAT ('NDXRIN: ERROR ',I3,' DETERMINING NUMBER OF ANTENNAS')
      END
      SUBROUTINE NDXRUV (IRET)
C-----------------------------------------------------------------------
C   NDXRUV indexes the data file, and writes a CL table if needed.
C   Input:
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
C                                       MAXSUB = max. no subarrays.
      INTEGER   MAXSUB, MXANTS
      PARAMETER (MAXSUB = 100)
      PARAMETER (MXANTS = MAXANT*MAXSUB)
      CHARACTER KEYS(2)*24, IFILE*48
      INTEGER   IRET, INIO, IPTRI, LUNI, INDI, IERR, ILENBU, IBIND, I,
     *   NXBUFF(512), NXKOLS(MAXNXC), NXNUMV(MAXNXC), SUBARR,
     *   CURSOU, CURFQI, LSTSOU(MAXSUB), NXLUN, MXSUB, NUMSUB, VER, I2,
     *   IVER, OVER, KEY(2,2), TIMKOL, SUBKOL, KOLS(2), NKEYA, LKEY,
     *   CLUN, CLBUFF(1024), CLKOLS(MAXCLC), CLNUMV(MAXCLC), CLVER,
     *   ICLRNO, NUMPOL, NUMIF, ANTUP(MAXANT,MAXSUB), IA1, IA2, BO,
     *   ILOCSC, VO, NUMVIS, INXRNO, VSTART(MAXSUB), VEND(MAXSUB),
     *   FREQID(MAXSUB), KBASE, NTERM, SCANNO, LSTSCN(MAXSUB), NOBEAM,
     *   KEYSUB(2,2)
      LOGICAL   T, F, FIRST(MAXSUB)
      REAL      DTIME, LSTIME(MAXSUB), OLDTIM(MAXSUB), STIME(MAXSUB),
     *   TLIM, SLIM, TCHANG, SCHANG, CURTIM, FKEY(2,2), TIMEC, GMMOD,
     *   BASEN, LSTCS(MAXSUB), CSCHG, OSDRA(MAXANT,MAXSUB),
     *   OSDDEC(MAXANT,MAXSUB), SDRA(MAXANT), SDDEC(MAXANT), INTERV
      INCLUDE 'INDXH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (KOLS(1), TIMKOL), (KOLS(2), SUBKOL)
      INCLUDE 'INCS:DIMV.INC'
      DATA NXLUN, CLUN, LUNI /30, 27, 25/
      DATA VO, BO /0, 1/
      DATA T, F, FIRST /.TRUE.,.FALSE.,MAXSUB*.TRUE./
      DATA NKEYA, LKEY /2,24/
      DATA KEYS /'TIME ','SUBARRAY '/
      DATA FKEY /1.0,0.0, 1.0,0.0/
      DATA KEYSUB /4*1/
      DATA ANTUP /MXANTS*0/
C-----------------------------------------------------------------------
      MXSUB = MAXSUB
C                                       Destroy previous NX tables
      CALL ZAPEXT ('NX', IRET)
C                                       SCAN pointer
      CALL AXEFND (4, 'SCAN    ', CATBLK(KIPCN), CATH(KHPTP), ILOCSC,
     *   IERR)
      IF (IERR.NE.0) ILOCSC = -1
      SCANNO = -1
      CURSOU = 1
C                                       Create/init index (NX) table
      VER = 1
      CALL NDXINI ('WRIT', NXBUFF, DISKIN, FCNO(NCFILE), VER, CATBLK,
     *   NXLUN, INXRNO, NXKOLS, NXNUMV, IRET)
      IF (IRET.NE.0) GO TO 999
      INXRNO = 1
C                                       Create/init cal (CL) table
      IF ((NEWCL) .OR. (NEWCS)) THEN
         CLVER = 1
         NUMPOL = MIN (CATBLK(KINAX+JLOCS), 2)
C                                       IQUV?
         IF (CATD(KDCRV+JLOCS).GT.0.0D0) NUMPOL = 1
         IF (JLOCIF.GT.-1) THEN
            NUMIF = CATBLK(KINAX+JLOCIF)
         ELSE
            NUMIF = 1
            END IF
         IF (NUMIF.GT.MAXIF) THEN
            WRITE (MSGTXT,1250) NUMIF
            IRET = 1
            GO TO 990
            END IF
         GMMOD = 1.0
         END IF
C                                       VLBA correlator produces
C                                       6th order polynomials,
C                                       hard-code here for now.
      IF (NEWCL) THEN
         NTERM = 6
         CALL CALINI ('WRIT', CLBUFF, DISKIN, FCNO(NCFILE), CLVER,
     *      CATBLK, CLUN, ICLRNO, CLKOLS, CLNUMV, NUMANT, NUMPOL,
     *      NUMIF, NTERM, GMMOD, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET, 'CREATING CL'
            GO TO 990
            END IF
         MSGTXT = 'Creating a new default CL table (version # 1)'
         CALL MSGWRT (3)
         IF (REDEL) THEN
            MSGTXT = 'Recalculating VLBA CL delays from IM table '
            CALL MSGWRT (3)
            END IF
         END IF
C                                       Create new CS table
      IF (NEWCS) THEN
         INTERV = CPARM(3) / 60. / 24.
         ICLRNO = 0
         NOBEAM = NUMANT
         CALL SDTCS (CURTIM, SUBARR, SDRA, SDDEC, NOBEAM, NUMPOL,
     *      NUMIF, DISKIN, OLDCNO, ANTUP, CATBLK, CLKOLS, CLNUMV,
     *      ICLRNO, CLBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET, 'CREATING CS'
            GO TO 990
            END IF
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      NUMSUB = 1
C                                       Setup for INDEX file
      TLIM = CPARM(1)
      IF (TLIM.LT.0.001) TLIM = 0.001
      TLIM = TLIM / (24.0 * 60.0)
      SLIM = CPARM(2)
      IF (SLIM.LT.0.001) SLIM = 0.001
      SLIM = SLIM / (24.0 * 60.0)
C                                       Find logical column numbers
C                                       in NX for time and subarray
      CALL FNDCOL (NKEYA, KEYS, LKEY, T, NXBUFF, KOLS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1040) IRET
         GO TO 990
         END IF
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
C                                       loop thru buffer full
         DO 180 I = 1,INIO
            NUMVIS = NUMVIS + 1
            IF (ILOCSU.GE.0) CURSOU = BUFF1(IPTRI+ILOCSU) + 0.5
            IF (ILOCSC.GE.0) SCANNO = BUFF1(IPTRI+ILOCSC) + 0.5
            CURTIM = BUFF1(IPTRI+ILOCT)
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN/256 + 0.1
               IA2 = BASEN - IA1*256 + 0.1
               KBASE = BASEN + 0.1
               SUBARR = (BASEN-KBASE) * 100.0 + 1.5
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               SUBARR = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            ANTUP(IA1,SUBARR) = 1
            ANTUP(IA2,SUBARR) = 1
            CURFQI = 1
            IF (ILOCFQ.GT.0) CURFQI = BUFF1(IPTRI+ILOCFQ)
            SDRA(IA2) = BUFF1(IPTRI+ILOCU)
            SDDEC(IA2) = BUFF1(IPTRI+ILOCV)
C                                       Init subarray data
            IF (FIRST(SUBARR)) THEN
               FIRST(SUBARR) = F
               VSTART(SUBARR) = NUMVIS
               LSTIME(SUBARR) = CURTIM
               LSTCS(SUBARR) = CURTIM
               OLDTIM(SUBARR) = LSTIME(SUBARR)
               STIME(SUBARR) = LSTIME(SUBARR)
               LSTSOU(SUBARR) = CURSOU
               LSTSCN(SUBARR) = SCANNO
               FREQID(SUBARR) = CURFQI
               NUMSUB = MAX (NUMSUB, SUBARR)
               CALL RCOPY (MAXANT, SDRA, SDRA)
               CALL RCOPY (MAXANT, SDDEC, SDDEC)
C                                       Too many subarrays
               IF (SUBARR.GT.MXSUB) THEN
                  IRET = 10
                  WRITE (MSGTXT,1020) SUBARR, MXSUB
                  GO TO 990
                  END IF
               IF (NEWCS) THEN
                  NOBEAM = NUMAN(SUBARR+1)
                  CALL SDTCS (CURTIM, SUBARR, SDRA, SDDEC, NOBEAM,
     *               NUMPOL, NUMIF, DISKIN, OLDCNO, ANTUP(1,SUBARR),
     *               CATBLK, CLKOLS, CLNUMV, ICLRNO, CLBUFF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1260) IRET, 'WRITING CS'
                     GO TO 990
                     END IF
                  END IF
               END IF
            TCHANG = CURTIM - LSTIME(SUBARR)
            SCHANG = CURTIM - STIME(SUBARR)
            CSCHG = CURTIM - LSTCS(SUBARR)
            IF ((SCANNO.NE.LSTSCN(SUBARR)) .OR. (TCHANG.GT.TLIM) .OR.
     *         (CURSOU.NE.LSTSOU(SUBARR)) .OR.(SCHANG.GT.SLIM) .OR.
     *         (CURFQI.NE.FREQID(SUBARR))) THEN
               TIMEC = (LSTIME(SUBARR) + OLDTIM(SUBARR)) * 0.5
               DTIME = LSTIME(SUBARR) - OLDTIM(SUBARR)
               CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *            TIMEC, DTIME, LSTSOU(SUBARR), SUBARR,
     *            VSTART(SUBARR), VEND(SUBARR), FREQID(SUBARR), IRET)
               IF (IRET.NE.0) GO TO 999
               IF (NEWCL) THEN
                  CALL WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, SUBARR,
     *               OLDTIM(SUBARR), LSTIME(SUBARR), NUMPOL, NUMIF,
     *               LSTSOU(SUBARR), FREQID(SUBARR), ANTUP, CPARM(3),
     *               IRET)
                  IF (IRET.NE.0) GO TO 999
                  END IF
               IF (NEWCS) THEN
                  NOBEAM = NUMAN(SUBARR+1)
C                                       end last scane
                  CALL SDTCS (LSTIME(SUBARR), SUBARR, OSDRA(1,SUBARR),
     *               OSDDEC(1,SUBARR), NOBEAM, NUMPOL, NUMIF, DISKIN,
     *               OLDCNO, ANTUP(1,SUBARR), CATBLK,  CLKOLS, CLNUMV,
     *               ICLRNO, CLBUFF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1260) IRET, 'WRITING CS'
                     GO TO 990
                     END IF
C                                       start new scan
                  CALL SDTCS (CURTIM, SUBARR, SDRA, SDDEC, NOBEAM,
     *               NUMPOL, NUMIF, DISKIN, OLDCNO, ANTUP(1,SUBARR),
     *               CATBLK, CLKOLS, CLNUMV, ICLRNO, CLBUFF, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1260) IRET, 'WRITING CS'
                     GO TO 990
                     END IF
                  END IF
               LSTCS(SUBARR) = CURTIM
               LSTSCN(SUBARR) = SCANNO
               OLDTIM(SUBARR) = CURTIM
               STIME(SUBARR) = CURTIM
               VSTART(SUBARR) = NUMVIS
               LSTSOU(SUBARR) = CURSOU
               FREQID(SUBARR) = CURFQI
               DO 135 I2 = 1,MAXANT
                  ANTUP(I2,SUBARR) = 0
 135              CONTINUE
            ELSE IF ((NEWCS) .AND. (CSCHG.GE.INTERV)) THEN
               NOBEAM = NUMAN(SUBARR+1)
               CALL SDTCS (CURTIM, SUBARR, SDRA, SDDEC, NOBEAM,
     *            NUMPOL, NUMIF, DISKIN, OLDCNO, ANTUP(1,SUBARR),
     *            CATBLK, CLKOLS, CLNUMV, ICLRNO, CLBUFF, IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1260) IRET, 'WRITING CS'
                  GO TO 990
                  END IF
               LSTCS(SUBARR) = CURTIM
               END IF
C                                       Reset last time
            LSTIME(SUBARR) = BUFF1(IPTRI+ILOCT)
            OSDRA(IA2,SUBARR) = BUFF1(IPTRI+ILOCU)
            OSDDEC(IA2,SUBARR) = BUFF1(IPTRI+ILOCV)
            IPTRI = IPTRI + LREC
            VEND(SUBARR) = NUMVIS
 180        CONTINUE
         GO TO 100
C                                       Flush current scan(s)
 200  DO 210 SUBARR = 1,NUMSUB
         TIMEC = (LSTIME(SUBARR) + OLDTIM(SUBARR)) * 0.5
         DTIME = LSTIME(SUBARR) - OLDTIM(SUBARR)
         IF ((.NOT.FIRST(SUBARR)) .AND.
     *      (VSTART(SUBARR).LE.VEND(SUBARR)))
     *      CALL TABNDX ('WRIT', NXBUFF, INXRNO, NXKOLS, NXNUMV,
     *      TIMEC, DTIME, LSTSOU(SUBARR), SUBARR,
     *      VSTART(SUBARR), VEND(SUBARR), FREQID(SUBARR), IRET)
         IF (IRET.NE.0) GO TO 999
         IF (NEWCL) THEN
            CALL WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, SUBARR,
     *         OLDTIM(SUBARR), LSTIME(SUBARR), NUMPOL, NUMIF,
     *         LSTSOU(SUBARR), FREQID(SUBARR), ANTUP, CPARM(3),
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
         IF (NEWCS) THEN
            NOBEAM = NUMAN(SUBARR+1)
            CALL SDTCS (LSTIME(SUBARR), SUBARR, OSDRA(1,SUBARR),
     *         OSDDEC(1,SUBARR), NOBEAM, NUMPOL, NUMIF, DISKIN, OLDCNO,
     *         ANTUP(1,SUBARR), CATBLK,  CLKOLS, CLNUMV, ICLRNO, CLBUFF,
     *         IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1260) IRET, 'WRITING CS'
               GO TO 990
               END IF
            END IF
 210     CONTINUE
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
C                                       Make sure number of records OK
      INXRNO = INXRNO - 1
      NXBUFF(5) = INXRNO
      CALL TABIO ('CLOS', 0, INXRNO, BUFF1, NXBUFF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1220) IRET
         GO TO 990
         END IF
      IF (NEWCL) CALL TABIO ('CLOS', 0, ICLRNO, BUFF1, CLBUFF, IRET)
C                                       Create new CS table
      IF (NEWCS) THEN
         ICLRNO = -1
         NOBEAM = NUMAN(SUBARR+1)
         CALL SDTCS (CURTIM, SUBARR, SDRA, SDDEC, NOBEAM, NUMPOL,
     *      NUMIF, DISKIN, OLDCNO, ANTUP, CATBLK, CLKOLS, CLNUMV,
     *      ICLRNO, CLBUFF, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1260) IRET, 'CLOSING CS'
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Sort NX table into time order
C                                       for multiple subarrays.
      IVER = 1
      OVER = 1
      KEY(1,1) = TIMKOL
      KEY(2,1) = TIMKOL
      KEY(1,2) = SUBKOL
      KEY(2,2) = SUBKOL
      IF (NUMSUB.GT.1) THEN
         CALL TABSRT (FVOL(NCFILE), FCNO(NCFILE), 'NX', IVER, OVER, KEY,
     *      KEYSUB, FKEY, NXBUFF, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1240) IRET
            GO TO 990
            END IF
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', FVOL(NCFILE), FCNO(NCFILE), CATBLK, 'REST',
     *   BUFF1, IERR)
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('NDXRUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('NDXRUV: TOO MANY SUBARRAYS ',I5,' > ',I5)
 1030 FORMAT ('NDXRUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1040 FORMAT ('NDXRUV: FNDCOL ERROR',I3)
 1100 FORMAT ('NDXRUV: ERROR',I3,' READING VIS FILE')
 1220 FORMAT ('NDXRUV: ERROR',I3,' CLOSING INDEX TABLE')
 1240 FORMAT ('NDXRUV: ERROR',I3,' SORTING INDEX TABLE')
 1250 FORMAT ('NDXRUV: #IFS = ',I6,' TOO MANY, CHECK CATBLK')
 1260 FORMAT ('NDXRUV: ERROR ',I3,1X,A,' TABLE')
      END
      SUBROUTINE ZAPEXT (EXTTYP, IERR)
C-----------------------------------------------------------------------
C   ZAPEXT destroys extension files.
C   Adapted from Lars Baath CLENUP.
C   Input:  EXTTYP   C*2  Extension file type
C   Output: IERR     I    Error code, 0+>OK, else failed.
C-----------------------------------------------------------------------
      CHARACTER EXTTYP*2, IBLNK*4, CHTM2*2, FNAME*48, REST*4
      INTEGER   IERR
      INTEGER   NFILES, IFILE, IV
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INDXH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IBLNK, REST /'    ','REST'/
C-----------------------------------------------------------------------
C                                       Find number of files
      NFILES = 0
      DO 10 IFILE = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+IFILE-1), CHTM2)
         IF (CHTM2.EQ.EXTTYP) THEN
            IV = IFILE
            NFILES = CATBLK(KIVER)
            END IF
 10      CONTINUE
      IF (NFILES.LE.0) GO TO 999
C                                       Destroy files
      DO 50 IFILE = 1,NFILES
         CALL ZPHFIL (EXTTYP, DISKIN, OLDCNO, IFILE, FNAME, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR
            GO TO 990
            END IF
         CALL ZDESTR (DISKIN, FNAME, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
 50      CONTINUE
C                                       Mark in header.
      CATBLK(KIVER+IV-1) = 0
      CALL CHR2H (2, IBLNK, 1, CATH(KHEXT+IV-1))
C                                       Update CATBLK on disk
      CALL CATIO ('UPDT', DISKIN, OLDCNO, CATBLK, REST, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1050) IERR
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('ZAPEXT: ERROR =',I3,' FROM ZPHFIL')
 1040 FORMAT ('ZAPEXT: ERROR =',I3,' FROM ZDESTR')
 1050 FORMAT ('ZAPEXT: ERROR =',I3,' UPDATING CATBLK')
      END
      SUBROUTINE WRITCL (CLBUFF, ICLRNO, CLKOLS, CLNUMV, SUBARR, STIME,
     *   LTIME, NUMPOL, NUMIF, CURSOU, CURFQI, ANTUP, USINT, IRET)
C-----------------------------------------------------------------------
C  Routine to write the CL entries required to cover the time range
C  defined by the NX entries.
C  Inputs:
C    CLBUFF(*)       I     CL table buffer
C    ICLRNO          I     Next CL row number to be written
C    CLKOLS(MAXCLC)  I     Array defining the data types in the
C                          CL columns.
C    CLNUMV(MAXCLC)  I     Array defining the number of entries per
C                          column
C    SUBARR          I     Subarray number
C    STIME           R     Start time of NX record
C    LTIME           R     Stop time of NX record
C    NUMPOL          I     # polzns in data
C    NUMIF           I     # IF's in data
C    CURSOU          I     Source number of scan
C    CURFQI          I     Freq id number of scan
C    ANTUP(IANT,ISUB) I    Array specifiying which antennas are used
C                          for which subarray.
C    USINT           R     User specified interval for CL entries
C  From COMMON:
C    REDEL           L     VLBA only: recalculate new CL entry group
C                          delays using IM table information. True
C                          => recalculate delays, False => don't.
C
C  Output:
C    IRET            I     Error code: 0 => OK
C-----------------------------------------------------------------------
      INTEGER CLBUFF(*), ICLRNO, CLKOLS(*), CLNUMV(*), SUBARR, CURSOU,
     *   CURFQI, NUMPOL, NUMIF, IRET
      REAL    STIME, LTIME, USINT
C
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   BUFFIM(512)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER MXIFRQ, MXIFR1, NUMREC
      PARAMETER (MXIFRQ = 25*MAXIF)
      PARAMETER (MXIFR1 = 4*MAXIF)
      INTEGER   ANTUP(MAXANT,*)
      INTEGER   REFA(2,MAXIF), NENT, IANT, JANT, I, LUN, INVER
      REAL      DOPOFF(MAXIF), ATMOS, DATMOS, MBDELY(2), CLOCK(2),
     *   DCLOCK(2), DISP(2), DDISP(2), CREAL(2,MAXIF), CIMAG(2,MAXIF),
     *   DELAY(2,MAXIF), RATE(2,MAXIF), WEIGHT(2,MAXIF),
     *   TIMER, INTERV
      DOUBLE PRECISION  GEODLY(10), DSTIME, DLTIME
      SAVE LUN, BUFFIM

      INCLUDE 'INDXH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DGLB.INC'
      INCLUDE 'INCS:DIMV.INC'
C
      DATA REFA /MAXIF*0, MAXIF*0/
      DATA GEODLY /10*0.0D0/
      DATA DOPOFF, MBDELY, CLOCK, DCLOCK /MAXIF*0.0, 6*0.0/
      DATA ATMOS, DATMOS, DISP, DDISP /6*0.0/
      DATA CIMAG, DELAY, RATE, WEIGHT /MAXIF*0.0, MAXIF*0.0,
     *   MAXIF*0.0, MAXIF*0.0, MAXIF*0.0, MAXIF*0.0, MAXIF*0.0,
     *   MAXIF*0.0/
      DATA CREAL /MAXIF*1.0, MAXIF*1.0/
      DATA LUN /38/
      DATA INVER /1/
C-----------------------------------------------------------------------
C                                       Determine time range of scan
C                                       in minutes
      TIMER = (LTIME - STIME) * 24. * 60.
      NENT = TIMER / USINT
      DSTIME = STIME
      DLTIME = LTIME
      INTERV = USINT / 60. / 24.
C                                       If first entry, initialize
C                                       IM model access
      IF (REDEL .AND. ( ICLRNO.EQ.1)) THEN
C                                       First, check is sorted.
         CALL IMSORT (DISKIN, OLDCNO, INVER, CATBLK, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1010) IRET
            GO TO 995
            END IF
         CALL IMINI ('READ', BUFFIM, DISKIN, OLDCNO, INVER, CATBLK, LUN,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT, 1100) IRET
            GO TO 995
            END IF
         NUMREC = 0
         END IF

C                                       Ensure entry written at
C                                       start of scan.
      JANT = NUMAN(SUBARR+1)
      DO 100 IANT = 1,JANT
         IF (ANTUP(IANT,SUBARR).EQ.1) THEN
            IF (REDEL) THEN
               CALL GETDEL (DSTIME, CURSOU, IANT, SUBARR,
     *            CURFQI, BUFFIM, GEODLY, NUMREC, IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *         NUMPOL, NUMIF, DSTIME, INTERV, CURSOU, IANT, SUBARR,
     *         CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *         CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *         WEIGHT, REFA, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
 100     CONTINUE
C                                       Write entries at specified
C                                       time intervals
      DO 400 I = 1,NENT
         DSTIME = DSTIME + INTERV
         IF (DSTIME.GT.DLTIME) GO TO 500
         DO 200 IANT = 1,NUMAN(SUBARR+1)
            IF (ANTUP(IANT,SUBARR).EQ.1) THEN
               IF (REDEL) THEN
                  CALL GETDEL (DSTIME, CURSOU, IANT, SUBARR, CURFQI,
     *               BUFFIM, GEODLY, NUMREC, IRET)
                  IF (IRET.NE.0) GO TO 990
                  END IF
               CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *            NUMPOL, NUMIF, DSTIME, INTERV, CURSOU, IANT, SUBARR,
     *            CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *            CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *            WEIGHT, REFA, IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
 200        CONTINUE
 400     CONTINUE
C                                       Check to see if we have entry
C                                       at end of scan
 500  IF (DSTIME.EQ.DLTIME) GO TO 999
C                                       Otherwise write entry for end
C                                       of scan
      DO 600 IANT = 1,NUMAN(SUBARR+1)
         IF (ANTUP(IANT,SUBARR).EQ.1) THEN
            IF (REDEL) THEN
               CALL GETDEL (DLTIME, CURSOU, IANT, SUBARR, CURFQI,
     *            BUFFIM, GEODLY, NUMREC, IRET)
               IF (IRET.NE.0) GO TO 990
               END IF
            CALL TABCAL ('WRIT', CLBUFF, ICLRNO, CLKOLS, CLNUMV,
     *         NUMPOL, NUMIF, DLTIME, INTERV, CURSOU, IANT, SUBARR,
     *         CURFQI, IFR, GEODLY, DOPOFF, ATMOS, DATMOS, MBDELY,
     *         CLOCK, DCLOCK, DISP, DDISP, CREAL, CIMAG, DELAY, RATE,
     *         WEIGHT, REFA, IRET)
            IF (IRET.NE.0) GO TO 990
            END IF
 600     CONTINUE
      GO TO 999
C
 990  WRITE (MSGTXT,1000) IRET
 995  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WRITCL: ERROR ',I3,' WRITING CL ENTRY')
 1010 FORMAT ('WRITCL: ERROR ',I3,' SORTING IM TABLE')
 1100 FORMAT ('WRITCL: ERROR ',I3,' OPENING IM TABLE')
      END
      SUBROUTINE SDTCS (TIME, SUBA, RA, DEC, NOBEAM, NUMPOL, NUMIF,
     *   DISK, CNO, ANTUP, CATBLK, CSKOLS, CSNUMV, ICSRNO, BUFFER, IRET)
C-----------------------------------------------------------------------
C   Routine to create/initialize a single dish calibration file (CS)
C   Table opened and closed each call.
C   Input:
C      TIME      R        Time (days)
C      SUBA      I        Subarray number.
C      RA        R(*)     RA (deg) 1 per beam
C      DEC       R(*)     Declination  (deg) 1 per beam
C      NOBEAM    I        Number of beams.
C      NUMPOL    I        Number of polarizations
C      NUMIF     I        Number of IFs
C      DISK      I        Disk number of file
C      CNO       I        Catalog slot number of file
C      ANTUP     I(*)     If > 0 => this antenna/beam exists
C      CATBLK    I(*)     Catalog header block
C   In/Out
C      CSKOLS    I(13)    Pointers
C      CSNUMV    I(13)    Column format
C      ICSRNO    I        Record number in file: in = 0 +> create,
C                         in = -1 => just close, else write
C   Output:
C      BUFFER(*) I     I/O buffer
C      IRET      I     Return code, 0=>OK, else TABCAL or CALINI error.
C-----------------------------------------------------------------------
      REAL      TIME, RA(*), DEC(*)
      INTEGER   SUBA, NOBEAM, NUMPOL, NUMIF, DISK, CNO, CATBLK(256),
     *   ANTUP(*), BUFFER(*), CSKOLS(13), CSNUMV(13), IRET
C
      INTEGER   VER, LUN, IBEAM, MUMBEM, MUMPOL, MUMIF, ICSRNO
      INCLUDE 'INCS:PUVD.INC'
      REAL      FAC(2,MAXIF), OFF(2,MAXIF), RAO(2,MAXIF), DECO(2,MAXIF)
      DATA VER /1/
      DATA        FAC,                 OFF
     *   /MAXIF*1.0,MAXIF*1.0, MAXIF*0.0,MAXIF*0.0/
      DATA        RAO,                 DECO
     *   /MAXIF*0.0,MAXIF*0.0, MAXIF*0.0,MAXIF*0.0/
C-----------------------------------------------------------------------
C                                       Open/create
      LUN = 26
      MUMBEM = NOBEAM
      MUMPOL = NUMPOL
      MUMIF = NUMIF
      IF (ICSRNO.EQ.0) THEN
         CALL CSINI ('WRIT', BUFFER, DISK, CNO, VER, CATBLK, LUN,
     *      ICSRNO, CSKOLS, CSNUMV, MUMBEM, MUMPOL, MUMIF, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Do beams
      ELSE IF (ICSRNO.GT.0) THEN
         DO 20 IBEAM = 1,NOBEAM
            IF (ANTUP(IBEAM).EQ.1) THEN
               CALL TABCS ('WRIT', BUFFER, ICSRNO, CSKOLS, CSNUMV,
     *            MUMPOL, TIME, RA(IBEAM), DEC(IBEAM), IBEAM, SUBA, FAC,
     *            OFF, RAO, DECO, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
 20         CONTINUE
C                                       Close
      ELSE
         CALL TABCS ('CLOS', BUFFER, ICSRNO, CSKOLS, CSNUMV, MUMPOL,
     *      TIME, RA, DEC, IBEAM, SUBA, FAC, OFF, RAO, DECO, IRET)
         END IF
C
 999  RETURN
      END

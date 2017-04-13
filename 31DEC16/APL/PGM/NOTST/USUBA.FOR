LOCAL INCLUDE 'USUBA.INC'
C                                       Local include for USUBA
C                                       Requires INCS:PUVD.INC
C                                       Input adverbs
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XSOUR(4,30), XINFIL(12),
     *   XOPCOD
      CHARACTER NAMEIN*12, CLAIN*6, SUBSRC(30)*16, LOPCOD*4,
     *   LINFIL*48
      REAL XSIN, XDISIN, XTIME(8), XANT(50), XFQID, XSUBA
      INTEGER SEQIN, DISKIN
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XOPCOD, XTIME,
     *   XANT, XSOUR, XFQID, XSUBA, XINFIL, SEQIN, DISKIN
      COMMON /CHRCOM/ NAMEIN, CLAIN, LOPCOD, SUBSRC, LINFIL
C                                       General parameters
      INTEGER OLDCNO
      COMMON /GENPAR/ OLDCNO
C                                       Buffers
      REAL      BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   JBUFSZ, IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
LOCAL INCLUDE 'DSUB.INC'
C                                       Subarray lookup table
C                                       Requires PUVD.INC
C                                       Max. no. entries
      INTEGER MAXENT
      PARAMETER (MAXENT = 100000)
C                                       Max. no. source entries
      INTEGER MAXSOU
      PARAMETER (MAXSOU = 300)
C                                       Max. size of lookup table
      INTEGER MAXLKP
      PARAMETER (MAXLKP = 99 * MAXANT + MAXANT)
C
      LOGICAL WSOPEN(MAXENT)
      REAL STIMEA(MAXENT), STIMEB(MAXENT)
      INTEGER ISANT(MAXANT,MAXENT), NSANT(MAXENT),
     *   ISSOUR(MAXSOU,MAXENT), NSSOUR(MAXENT), ISFQD(MAXENT),
     *   ISOLD(MAXENT), ISSUBA(MAXENT), ISANDX(MAXLKP), NSENTR,
     *   NSANDX, NXTSUB, NMXSUB, NSADD
      COMMON /SUBDAT/ STIMEA, STIMEB, ISANT, NSANT, ISSOUR, NSSOUR,
     *   ISFQD, ISOLD, ISSUBA, ISANDX, NSANDX, NSENTR, NXTSUB,
     *   NMXSUB, NSADD, WSOPEN
LOCAL END
      PROGRAM USUBA
C-----------------------------------------------------------------------
C! Assign subarrays within a uv-data file
C# UV-util Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999-2001, 2007, 2011, 2013, 2015
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
C   Task to assign subarrays within a uv-data file
C   INNAME           Input UV file name (name)
C   INCLASS          Input UV file name (class)
C   INSEQ            Input UV file name (seq. #)
C   INDISK           Input UV file disk unit #
C   OPCODE           Opcode ('AUTO', 'SCAN' or 'UPDT')
C   TIMERANG         Time range to use. 0=>all
C   ANTENNAS         Antenna numbers 0=>all
C   SOURCE           Source list ' '=> any
C   FREQID           Freq. id. 0=> any.
C   SUBARRAY         Subarray, 0=>next
C   INFILE           External text file of subarray assignments
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET, JLUN1, JLUN2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'USUBA.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM / 'USUBA '/
      DATA JLUN1, JLUN2 / 27, 28/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       perform initialization.
      CALL USUBIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Change subarray in uv-data.
C                                       unless scan only requested
      IF (LOPCOD.NE.'SCAN') THEN
         CALL SUBUV (IRET)
         IF (IRET.NE.0) GO TO 990
C                                       Update tables
         CALL TBUPDT (JLUN1, JLUN2, IRET)
         IF (IRET.NE.0) GO TO 990
      ELSE
C                                       Print out scan results
         CALL PRTSUB (IRET)
         END IF
C                                       History
      CALL USUHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, IBUFF1)
C
 999  STOP
      END
      SUBROUTINE USUBIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   USUBIN gets input parameters for USUBA
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in USUBA for more details.
C-----------------------------------------------------------------------
      CHARACTER PRGN*6
      INTEGER JERR
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER STAT*4, UTYPE*2
      INTEGER   IROUND, NPARM, IERR, LOOP, I, J, NP, ILUNF, IPRT
      LOGICAL   T, DOSWNT, WUNIQ
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T /.TRUE./
      DATA ILUNF, IPRT /10, 1/
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
      NPARM = 200
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) THEN
            GO TO 999
         ELSE
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, IBUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
      WRITE (MSGTXT,4000)
      CALL MSGWRT (2)
C                                       Get input file name, class.
      SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
C                                       Read CATBLK for input file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'WRIT', IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Update /CFILES/
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 1
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
C                                       Compile lookup table
C                                       of antenna numbers in
C                                       each subarray
      CALL ANINDX (DISKIN, OLDCNO, CATBLK, BUFF1, ISANDX, NSANDX,
     *   NXTSUB, WUNIQ, IERR)
      IF (IERR.NE.0) THEN
         JERR = 1
         WRITE (MSGTXT,1180) IERR
         GO TO 990
         END IF
C                                       Abort if existing AN
C                                       tables are inhomogeneous
C                                       (i.e. have different
C                                       antenna numbering or
C                                       ref. dates/frequencies)
      IF (.NOT.WUNIQ) THEN
         JERR = 2
         WRITE (MSGTXT,1190)
         GO TO 990
         END IF
C                                       Initialize count of
C                                       highest subarray
      NMXSUB = 0
C                                       Opcode
      CALL H2CHR (4, 1, XOPCOD, LOPCOD)
C                                       Case opcode of:
C                                       1: Automatic subarray
C                                          identification.
      IF ((LOPCOD.NE.'AUTO').AND.(LOPCOD.NE.'SCAN')) GO TO 200
C                                       Fill subarray lookup
C                                       table automatically.
         CALL AUTOSB (JERR)
         IF (JERR.NE.0) GO TO 999
         GO TO 900
C                                       2: User-specified subarray
C                                          assignments.
200   CONTINUE
C                                       Is there an external file
C                                       of subarray assignments ?
      CALL H2CHR (48, 1, XINFIL, LINFIL)
      CALL CHBLNK (48, 1, LINFIL, NP)
      IF (NP.GT.0) THEN
C                                       Fill the subarray lookup
C                                       table from the external
C                                       text file.
         CALL EXTSUB (DISKIN, OLDCNO, LINFIL, ILUNF, IPRT, BUFF1, JERR)
         IF (JERR.NE.0) GO TO 999
      ELSE
C                                       Fill the subarray lookup
C                                       table with the input adverbs.
C                                       In this case lookup table
C                                       contains only one entry.
         NSENTR = 1
C                                       Timerange
         STIMEA(1) = XTIME(1) + (1.0/24.0) * (XTIME(2) + (1.0/60.0) *
     *      (XTIME(3) + (1.0/60.0) * XTIME(4)))
         STIMEB(1) = XTIME(5) + (1.0/24.0) * (XTIME(6) + (1.0/60.0) *
     *      (XTIME(7) + (1.0/60.0) * XTIME(8)))
C                                       Antenna numbers
         CALL FILL (MAXANT, 0, ISANT)
         NSANT(1) = 0
         DO 100 LOOP = 1, 50
            J = IROUND (XANT(LOOP))
            IF (J.GT.0) CALL APPNDA (J, ISANT(1,1), NSANT(1))
 100        CONTINUE
C                                       Source names
         DO 25 I = 1, 30
            CALL H2CHR (16, 1, XSOUR(1,I), SUBSRC(I))
   25       CONTINUE
C                                       Assign source numbers
         CALL FNDSOU (DISKIN, OLDCNO, SUBSRC, IBUFF1, NSSOUR(1),
     *      DOSWNT, ISSOUR(1,1), JERR)
         IF (IERR.NE.0) GO TO 999
C                                       Freq. id.
         ISFQD(1) = IROUND (XFQID)
C                                       New subarray number
         ISSUBA(1) = IROUND (XSUBA)
C                                       Old subarray number:
C                                       not available as an adverb
         ISOLD(1) = 0
         END IF
         GO TO 900
C                                       Endcase: OPCODE
900   CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('USUBIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1180 FORMAT ('USUBIN: ERROR',I3,' READING AN TABLES')
 1190 FORMAT ('CANNOT YET DEAL WITH INHOMOGENEOUS AN TABLES')
 4000 FORMAT ('You are using a non-standard program')
      END
      SUBROUTINE SUBUV (IRET)
C-----------------------------------------------------------------------
C   SUBUV changes the subarray of the data.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, LRECO,
     *   ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, ISBNEW
      LOGICAL   T, F
      INTEGER   BO, VO, NUMVIS, XCOUNT, IERR
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Print message
      WRITE (MSGTXT,1000)
      CALL MSGWRT (6)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1005) IRET
         GO TO 990
C                                       Open vis file for write
 10   CALL ZOPEN (LUNO, INDO, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 20
         WRITE (MSGTXT,1010) IRET
         GO TO 990
C                                       Init vis file for write
C                                       LRECO = length of output rec.
 20   LRECO = LREC
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
 30   IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         DO 190 I = 1,INIO
C                                       Change subarray if selected
            CALL CHGSUB (BUFF1(IPTRI), ISBNEW, IERR)
C                                       Update count of highest
C                                       subarray writen
            NMXSUB = MAX (NMXSUB, ISBNEW)
            NUMVIS = NUMVIS + 1
C                                       Copy to output
            CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            IPTRI = IPTRI + LREC
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
         IF (INIO.GT.0) GO TO 100
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Updating subarrays in uv-data')
 1005 FORMAT ('SUBUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('SUBUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SUBUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SUBUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SUBUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('SUBUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE AUTOSB (IRET)
C----------------------------------------------------------------------
C   Determine subbarrays automatically
C   Output:
C      IRET    I     Return code (0 => ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER IFILE*48
      REAL TINT, EPS
      INTEGER   INIO, IPTRI, LUNI, INDI, ILENBU, IBIND, I, BO, VO,
     *   NUMVIS, XCOUNT, IERR
      LOGICAL   T, F
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI /16/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Message
      WRITE (MSGTXT,1000)
      CALL MSGWRT (6)
C                                       Open visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1005) IRET
         GO TO 990
         END IF
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IRET
         GO TO 990
 40   NUMVIS = 0
      XCOUNT = 0
C                                       Initialize automatic
C                                       subarray assignment
      NSENTR = 0
      TINT = 0.0
C                                       Set EPS to accomodate
C                                       rounding errors
      EPS = 1.0E-6
C                                       Loop
C                                       Read vis. record.
 100     CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.EQ.0) GO TO 110
            WRITE (MSGTXT,1100) IRET
            GO TO 990
 110     IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
C                                       Accumulate subarray
C                                       lookup table automatically
            CALL SUBID (BUFF1(IPTRI), TINT, EPS, IRET)
            IF (IRET.NE.0) GO TO 999
C
            NUMVIS = NUMVIS + 1
            IPTRI = IPTRI + LREC
 190        CONTINUE
         GO TO 100
C                                       EOF
 200     CONTINUE
C                                       Process last subarray entry
      CALL SUBUPD (IERR)
C                                       Close file
      CALL ZCLOSE (LUNI, INDI, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Determining subarray assignments automatically')
 1005 FORMAT ('AUTOSB: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1030 FORMAT ('AUTOSB: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('AUTOSB: ERROR',I3,' READING VIS FILE')
      END
      SUBROUTINE SUBID (RANPRM, TINT, EPS, IRET)
C----------------------------------------------------------------------
C   Accumulate the subarray identification table record by record
C   Inputs:
C      RANPRM   R(*)   Random parameter array
C      TINT     R      Integration time if INDXIT <= 0.
C      EPS      R      Overlap allowed before trigering
C                      subarray condition (0..1)
C   Output:
C      IRET     I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      REAL      RANPRM(*), TINT, EPS
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      REAL      TIME, TINTV, TA, TB, TMAX
      INTEGER   IBASL, IA(2), ISOU, IFQID, JPTR, JMATCH, ISUB, IERR
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Extract random parameter values
      IF (ILOCB.GE.0) THEN
         IBASL = RANPRM(ILOCB+1) + 0.1
         IA(1) = IBASL / 256
         IA(2) = IBASL - IA(1) * 256
         ISUB = (RANPRM(ILOCB+1) - IBASL) * 100.0 + 1.5
      ELSE
         IA(1) = RANPRM(ILOCA1+1) + 0.1
         IA(2) = RANPRM(ILOCA2+1) + 0.1
         ISUB = RANPRM(ILOCSA+1) + 0.1
         END IF
      TIME = RANPRM(ILOCT+1)
C                                       Default data integration time
C                                       of one second if none specified
C                                       and no random parameter present
      TINTV = TINT
      IF (TINTV.LE.0.0) TINTV = 1.0
      IF (ILOCIT.GE.0) TINTV = RANPRM(ILOCIT+1)
C                                       Conver to days
      TINTV = TINTV / 86400.0
      ISOU = 1
      IF (ILOCSU.GE.0) ISOU = RANPRM(ILOCSU+1)
      IFQID = 1
      IF (ILOCFQ.GE.0) IFQID = RANPRM(ILOCFQ+1)
C                                       Upper, lower time limits from
C                                       current record (in days)
      TA = TIME - TINTV / 2.0
      TB = TIME + TINTV / 2.0
C                                       At beginning of current
C                                       accumulation ? Work space
C                                       at end of lookup table is
C                                       empty in this case.
      IF (NSENTR.EQ.NSADD) TMAX = TB
C                                       At end of accumulation ?
C                                       Incorporate overlap
C                                       condition in test.
      IF ((TMAX-TA).LE.(EPS*TINTV)) THEN
C                                       Consolidate entries in
C                                       work area in lookup table
         CALL SUBUPD (IERR)
C                                       Start new accumulation
         TMAX = TB
         NSADD = NSENTR
         END IF
C                                       Add current entry to work
C                                       area at end of lookup table
      JMATCH = 0
C                                       Try to match current baseline
C                                       to existing work entries.
      DO 400 JPTR = (NSENTR+1), NSADD
C                                       Require fq.id. and source id.
C                                       to match
         IF ((ISFQD(JPTR).EQ.IFQID).AND.(ISSOUR(1,JPTR).EQ.ISOU)) THEN
            JMATCH = JPTR
            END IF
400      CONTINUE
C                                       If match found then append
C                                       antennas to existing entry in
C                                       work area
      IF (JMATCH.GT.0) THEN
         CALL APPNDA (IA(1), ISANT(1,JMATCH), NSANT(JMATCH))
         CALL APPNDA (IA(2), ISANT(1,JMATCH), NSANT(JMATCH))
C                                       Update end time (necessary
C                                       for inhomogeneous integration
C                                       times or rounding errors)
         STIMEB(JMATCH) = TB
      ELSE
C                                       Else create new entry in
C                                       work space
         NSADD = NSADD + 1
C                                       Check buffer size
         IF (NSADD.GT.MAXENT) THEN
            IRET = 1
            WRITE (MSGTXT,1450)
            GO TO 990
            END IF
C
         ISANT(1,NSADD) = IA(1)
         NSANT(NSADD) = 1
         CALL APPNDA (IA(2), ISANT(1,NSADD), NSANT(NSADD))
         ISSOUR(1,NSADD) = ISOU
         NSSOUR(NSADD) = 1
         ISFQD(NSADD) = IFQID
         ISOLD(NSADD) = 0
C                                       Set default subarray numbers;
C                                       revised in SUBUPD
         ISSUBA(NSADD) = NSADD - NSENTR
         WSOPEN(NSADD) = .TRUE.
         STIMEA(NSADD) = TA
         STIMEB(NSADD) = TB
         END IF
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1450  FORMAT ('SUBID: PARAMETER MAXENT TOO SMALL. CONTACT AIPS ADMIN.')
      END
      SUBROUTINE SUBUPD (IRET)
C----------------------------------------------------------------------
C   Consolidate subarray lookup table at end of accumulation period
C   Output:
C      IRET     I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DSUB.INC'
      LOGICAL WMATCH, WFOUND
      DOUBLE PRECISION DQUAL, DQMAX(99), DTOP
      INTEGER JPTR, JBACK, I, J, K, JREL, NMATCH, ISMTCH(99),
     *   JSUBA, JMATCH, KPTR, ISVAL(99), NEWLIM, JANT
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      CALL DFILL (99, 0.0D0, DQMAX)
      CALL FILL (99, 0, ISMTCH)
      CALL FILL (99, 0, ISVAL)
C                                       Find best match for subarrays
C                                       found in this accumulation
C                                       period to existing subarray
C                                       selection entries.
C
C                                       Loop over all entries in
C                                       current accumulation period
C                                       (appended in work area of
C                                       subarray lookup table)
      DO 300 JPTR = (NSENTR+1), NSADD
         JREL = JPTR - NSENTR
C                                       Search backwards in time for
C                                       best match. Stop if perfect
C                                       match found.
         WMATCH = .FALSE.
         JBACK = NSENTR
C                                       While (still entries) and
C                                       (perfect match not yet found)
C                                       do check entry
150      IF ((JBACK.EQ.0).OR.(WMATCH)) GO TO 300
C                                       How many antennas match ?
            NMATCH = 0
            DO 225 I = 1, NSANT(JPTR)
               WFOUND = .FALSE.
               DO 200 K = 1, NSANT(JBACK)
                  IF (ISANT(I,JPTR).EQ.ISANT(K,JBACK)) WFOUND = .TRUE.
200               CONTINUE
               IF (WFOUND) NMATCH = NMATCH + 1
225            CONTINUE
C                                       Is this a perfect match to
C                                       an open entry (ie. a preceding
C                                       entry that can be extended in
C                                       time) ?
            WMATCH = ((NMATCH.EQ.NSANT(JPTR)).AND.
     *         (NSANT(JPTR).EQ.NSANT(JBACK)).AND.
     *         (ISFQD(JPTR).EQ.ISFQD(JBACK)).AND.
     *         (ISSOUR(1,JPTR).EQ.ISSOUR(1,JBACK)).AND.
     *         (WSOPEN(JBACK)))
C                                       If not, compute quality factor
C                                       of match (in range [0,1])
            IF (.NOT.WMATCH) THEN
               DQUAL = 0.0D0
               IF (ISFQD(JPTR).EQ.ISFQD(JBACK)) DQUAL = DQUAL + 1.0
               IF (ISSOUR(1,JPTR).EQ.ISSOUR(1,JBACK)) DQUAL =
     *            DQUAL + 1.0
               IF (NSANT(JBACK).GT.0) DQUAL = DQUAL +
     *            NMATCH / DBLE (NSANT(JBACK))
C                                       Normalize
               DQUAL = DQUAL / 3.0D0
C                                       Weight linearly by separation
C                                       in time
               DQUAL = DQUAL * (1.0 - (JPTR - JBACK) / DBLE (NSADD-1))
C                                       Update maximum for this
C                                       entry
               IF (DQUAL.GT.DQMAX(JREL)) THEN
                  DQMAX(JREL) = DQUAL
                  ISMTCH(JREL) = JBACK
                  END IF
C                                       Mark a perfect match
            ELSE
               DQMAX(JREL) = 999.0
               ISMTCH(JREL) = JBACK
               END IF
C                                       Decrement pointer
            JBACK = JBACK - 1
            GO TO 150
C                                       Endwhile
300      CONTINUE
C                                       Assign subarray numbers
C                                       for current accumulation
C                                       period; those that match
C                                       more closely processed
C                                       first.
      DO 500 J = 1, (NSADD-NSENTR)
C                                       Find highest match remaining
         DTOP = 0.0D0
         JPTR = 0
         DO 350 K = (NSENTR+1), NSADD
            I = K - NSENTR
C                                       Skip entries already processed
            IF (DQMAX(I).LT.0.0) GO TO 350
C                                       Update maximum
            IF (DQMAX(I).GT.DTOP) THEN
               DTOP = DQMAX(I)
               JPTR = K
               END IF
350         CONTINUE
C                                       Exit if no entries remaining
         IF (JPTR.EQ.0) GO TO 500
C                                       Highest remaining match found;
C                                       assign subarray number
         JREL = JPTR - NSENTR
         JMATCH = ISMTCH(JREL)
C                                       Exit if no match
         IF (JMATCH.EQ.0) GO TO 500
C                                       Is matched subarray number
C                                       already assigned for this
C                                       accumulation period ?
         JSUBA = ISSUBA(JMATCH)
         IF (ISVAL(JSUBA).GT.0) THEN
C                                       Find next free subarray no.
            JSUBA = 0
            K = 1
375         IF ((K.GT.99).OR.(JSUBA.GT.0)) GO TO 400
               IF (ISVAL(K).EQ.0) JSUBA = K
               K = K + 1
               GO TO 375
C
400         CONTINUE
            END IF
C                                       If perfect match then absorb
C                                       work entry into existing
C                                       record
         IF (DQMAX(JREL).GT.1.0) THEN
            STIMEB(JMATCH) = STIMEB(JPTR)
C                                       Else assign subarray label for
C                                       this entry in the work area
         ELSE
            ISSUBA(JPTR) = JSUBA
            END IF
C                                       Mark subarray number as used
         ISVAL(JSUBA) = 1
C                                       Mark work entry as processed
         DQMAX(JREL) = MIN (-DQMAX(JREL), -1.0D0)
500      CONTINUE
C
C                                       Consolidate work area; remove
C                                       entries absorbed as perfect
C                                       matches.
      JPTR = NSENTR + 1
      NEWLIM = NSADD
C                                       While still work entries do:
C                                       consolidate null records
550   IF (JPTR.GT.NEWLIM) GO TO 600
         JREL = JPTR - NSENTR
         IF (DQMAX(JREL).LT.-1.0) THEN
C                                       Delete work entry; move other
C                                       entries down.
            DO 575 KPTR = (JPTR+1), NEWLIM
               STIMEA(KPTR-1) = STIMEA(KPTR)
               STIMEB(KPTR-1) = STIMEB(KPTR)
               CALL COPY (NSANT(KPTR), ISANT(1,KPTR), ISANT(1,KPTR-1))
               NSANT(KPTR-1) = NSANT(KPTR)
               CALL COPY (NSSOUR(KPTR), ISSOUR(1,KPTR),
     *            ISSOUR(1,KPTR-1))
               NSSOUR(KPTR-1) = NSSOUR(KPTR)
               ISFQD(KPTR-1) = ISFQD(KPTR)
               ISOLD(KPTR-1) = ISOLD(KPTR)
               ISSUBA(KPTR-1) = ISSUBA(KPTR)
               WSOPEN(KPTR-1) = WSOPEN(KPTR)
               DQMAX(KPTR-NSENTR-1) = DQMAX(KPTR-NSENTR)
575            CONTINUE
C                                       Decrement work area record
C                                       count
            NEWLIM = NEWLIM - 1
         ELSE
C                                       Increment ptr. if record
C                                       not deleted.
            JPTR = JPTR + 1
            END IF
            GO TO 550
C                                       Endwhile
600      CONTINUE
C                                       Check for subarray selection
C                                       entries that need to be closed
      DO 800 JPTR = (NSENTR+1), NEWLIM
C                                       Loop over antennas in the
C                                       new entry; close earlier
C                                       entries using any antenna in
C                                       the new entry.
         DO 750 JANT = 1, NSANT(JPTR)
C                                       Search backwards
            DO 725 J = 1, NSENTR
               JBACK = NSENTR - J + 1
C                                       Skip if already closed
               IF (.NOT.WSOPEN(JBACK)) GO TO 725
C                                       Does this open entry contain
C                                       a matching antenna ?
                WFOUND = .FALSE.
                DO 700 K = 1, NSANT(JBACK)
                   IF (ISANT(K,JBACK).EQ.ISANT(JANT,JPTR))
     *                WFOUND = .TRUE.
700                CONTINUE
C                                       Close this entry if a
C                                       matching antenna found.
                IF (WFOUND) WSOPEN(JBACK) = .FALSE.
725            CONTINUE
750         CONTINUE
800      CONTINUE
C                                       Update count of valid
C                                       entries; reset work
C                                       area
      NSENTR = NEWLIM
      NSADD = NSENTR
C                                       Exit
999   RETURN
      END
      SUBROUTINE APPNDA (IADD, IARRAY, N)
C----------------------------------------------------------------------
C   Add an element to an integer array, avoiding duplication
C   Input:
C      IADD     I       Element to add.
C   Input/Output:
C      IARRAY   I(*)    Array
C      N        I       No. of elements in IARRAY
C----------------------------------------------------------------------
      INTEGER IADD, N, IARRAY(*)
C
      LOGICAL WFOUND
      INTEGER I
C----------------------------------------------------------------------
      WFOUND = .FALSE.
C                                       Search for existing copy
      DO 100 I = 1, N
         IF (IARRAY(I).EQ.IADD) WFOUND = .TRUE.
100      CONTINUE
C                                       Append
      IF (.NOT.WFOUND) THEN
         N = N + 1
         IARRAY(N) = IADD
         END IF
C                                       Exit
      RETURN
      END
      SUBROUTINE CHGSUB (RANPRM, ISBNEW, IRET)
C----------------------------------------------------------------------
C   Change subarray random parameter if selected.
C   Inputs:
C      RANPRM      R(*)     Array of random parameters
C   Output:
C      ISBNEW      I        Subarray on output
C      IRET        I        Return code
C                           0: consistent match found for both
C                              antennas (subarray changed).
C                           1: ant_1 and ant_2 yield different
C                              subarray numbers (=> no change).
C                           2: no match found (=> no change).
C----------------------------------------------------------------------
      REAL      RANPRM(*)
      INTEGER   ISBNEW, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DUVH.INC'
      REAL      TIME, XBASL
      INTEGER   IBASL, IFQID, IA(2), ISUB, ISOU, J, ISREPL(2)
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Extract random parameters
      IF (ILOCB.GE.0) THEN
         XBASL = RANPRM(ILOCB+1)
         IBASL = XBASL + 0.1
         IA(1) = XBASL / 256.0 + 0.1
         IA(2) = XBASL - IA(1) * 256.0 + 0.1
         ISUB = (XBASL - IBASL) * 100.0 + 1.5
      ELSE
         IA(1) = RANPRM(1+ILOCA1) + 0.1
         IA(2) = RANPRM(1+ILOCA2) + 0.1
         ISUB = RANPRM(1+ILOCSA) + 0.1
         END IF
      ISBNEW = ISUB
      TIME = RANPRM(ILOCT+1)
      IF (ILOCSU.GE.0) THEN
         ISOU = RANPRM(ILOCSU+1) + 0.1
      ELSE
         ISOU = 0
         END IF
      IF (ILOCFQ.GE.0) THEN
         IFQID = RANPRM(ILOCFQ+1) + 0.1
      ELSE
         IFQID = 0
         END IF
C                                       Search subarray selection
C                                       tables in common
      CALL FILL (2, 0, ISREPL)
      DO 400 J = 1, 2
         CALL FNDSUB (TIME, IA(J), ISUB, ISOU, IFQID, ISREPL(J))
400      CONTINUE
C                                       Do both antennas match
C                                       consistently ?
      IF (ISREPL(1).NE.ISREPL(2)) IRET = 1
      IF ((ISREPL(1).EQ.0).AND.(ISREPL(2).EQ.0)) IRET = 2
C                                       Change subarray if no
C                                       error; return new
C                                       subarray number
      IF (IRET.EQ.0) THEN
         IF (ILOCB.GE.0) THEN
            RANPRM(ILOCB+1) = XBASL + 0.01 * (ISREPL(1) - ISUB)
         ELSE
            RANPRM(ILOCSA+1) = ISREPL(1)
            END IF
         ISBNEW = ISREPL(1)
         END IF
C                                       Exit
      RETURN
      END
      SUBROUTINE FNDSUB (TIME, IANT, ISUB, ISOU, IFQID, ISNEW)
C----------------------------------------------------------------------
C   Search the subarray selection common for a match
C   Input:
C      TIME      R      Time (days wrt reference day)
C      IANT      I      Antenna number
C      ISUB      I      Subarray number
C      ISOU      I      Source number
C      IFQID     I      Freq. id. number
C   Output:
C      ISNEW     I      New subarray number (0 => no match)
C----------------------------------------------------------------------
      REAL TIME
      INTEGER IANT, ISUB, ISOU, IFQID, ISNEW
C
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL WMATCH
      INTEGER I, J
      INCLUDE 'DSUB.INC'
C----------------------------------------------------------------------
C                                       Initialization
      ISNEW = 0
C                                       Search selection common
      DO 300 J = 1, NSENTR
C                                       Matching times ?
         IF (((STIMEA(J).NE.0.0).OR.(STIMEB(J).NE.0.0)).AND.
     *      (TIME.GT.0.0).AND.((TIME.LT.STIMEA(J)).OR.
     *      (TIME.GT.STIMEB(J)))) GO TO 300
C                                       Matching antennas ?
         WMATCH = .TRUE.
         IF ((NSANT(J).GT.0).AND.(IANT.GT.0)) THEN
            WMATCH = .FALSE.
            DO 220 I = 1, NSANT(J)
               IF (ISANT(I,J).EQ.IANT) WMATCH = .TRUE.
220            CONTINUE
            END IF
         IF (.NOT.WMATCH) GO TO 300
C                                       Matching sources ?
         WMATCH = .TRUE.
         IF ((NSSOUR(J).GT.0).AND.(ISOU.GT.0)) THEN
            WMATCH = .FALSE.
            DO 240 I = 1, NSSOUR(J)
               IF (ISSOUR(I,J).EQ.ISOU) WMATCH = .TRUE.
240            CONTINUE
            END IF
         IF (.NOT.WMATCH) GO TO 300
C                                       Matching freq. id ?
         IF ((ISFQD(J).GT.0).AND.(IFQID.GT.0).AND.
     *      (ISFQD(J).NE.IFQID)) GO TO 300
C                                       Matching existing subarray ?
         IF (((ISOLD(J).GT.0).AND.(ISOLD(J).NE.ISUB)).AND.
     *      (ISUB.GT.0)) GO TO 300
C                                       Match found
         IF (ISSUBA(J).LE.0) THEN
            ISNEW = NXTSUB
         ELSE
            ISNEW = ISSUBA(J)
            END IF
300      CONTINUE
C                                       Exit
      RETURN
      END
      SUBROUTINE EXTSUB (IDISK, ICNO, LFILE, JLUN, IPRT, BUFFER, IRET)
C----------------------------------------------------------------------
C   Read subarray selection parameters from an external text file
C   Input:
C      IDISK    I       Disk volume number
C      ICNO     I       Catalog slot number
C      LFILE    C*48    External file name
C      JLUN     I       LUN for text file I/O
C      IPRT     I       Print level
C      BUFFER   R(512)  Table I/O buffer
C   Output:
C      IRET     I       Return code (0 => ok; else error)
C----------------------------------------------------------------------
      CHARACTER LFILE*48
      REAL BUFFER(*)
      INTEGER IDISK, ICNO, JLUN, IPRT, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INTEGER NPARS
      PARAMETER (NPARS = 2 * MAXANT + MAXSOU + 11)
      LOGICAL WERROR, WEOF, WNTSRC
      CHARACTER LPARS(NPARS)*8, LVALS(NPARS)*8, LMARK*8, LKEY*8,
     *   LSOURC(MAXSOU)*16
      DOUBLE PRECISION DVALS(NPARS)
      REAL TIME(4), TDAY
      INTEGER IFINDF, IERR, I, J, K, M, KMODE, IANT
      INTEGER IKEY
      DATA LPARS / MAXANT*'ANTENNAS', MAXANT*'ANT_NAME',
     *   MAXSOU*'SOURCES ', 8*'TIMERANG', 'FREQID  ', 'SUBARRAY',
     *   'OLD_SUBA'/
      DATA LMARK /'/       '/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NSENTR = 0
C                                       Open the external text file
      CALL ZTXOPN ('READ', JLUN, IFINDF, LFILE, .FALSE., IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR, LFILE
         IRET = 1
         GO TO 990
         END IF
C                                       While (NOT (EOF or ERROR)) do
C                                       read record;
100   CONTINUE
C                                       Set defaults
      DO 120 J = 1, NPARS
         DVALS(J) = DBLANK
         LVALS(J) = '        '
120      CONTINUE
C                                       Echo KEYIN input if
C                                       verbose print level selected
      KMODE = 0
      IF (IPRT.GT.0) KMODE = 1
C
      CALL KEYIN (LPARS, DVALS, LVALS, NPARS, LMARK, KMODE, JLUN,
     *   IFINDF, IERR)
      WEOF = (IERR.EQ.1)
      WERROR = (IERR.NE.0).AND.(.NOT.WEOF)
      IF (WERROR) THEN
         IRET = 2
         WRITE (MSGTXT,1120) IERR
         GO TO 900
         END IF
C                                       EOF or ERROR detected
      IF (WEOF.OR.WERROR) GO TO 900
C                                       Process KEYIN record
      NSENTR = NSENTR + 1
C                                       Check buffer size
      IF (NSENTR.GT.MAXENT) THEN
         IRET = 3
         WRITE (MSGTXT,1160)
         GO TO 990
         END IF
C                                       Antenna numbers
      LKEY = 'ANTENNAS'
      J = IKEY (LKEY, LPARS, NPARS)
      NSANT(NSENTR) = 0
      DO 200 I = 1, MAXANT
         K = J + I - 1
         IF (DVALS(K).NE.DBLANK) THEN
            IANT = DVALS(K) + 0.1
            IF (IANT.GT.0) CALL APPNDA (IANT, ISANT(1,NSENTR),
     *         NSANT(NSENTR))
            END IF
200      CONTINUE
C                                       Antenna names - NYI
C
C                                       Source names
      LKEY = 'SOURCES'
      J = IKEY (LKEY, LPARS, NPARS)
      NSSOUR(NSENTR) = 0
      M = 0
      DO 300 I = 1, MAXSOU
         LSOURC(I) = ' '
         K = J + I - 1
         IF (DVALS(K).NE.DBLANK) THEN
            M = M + 1
            LSOURC(M) = LVALS(K)
            END IF
300      CONTINUE
C                                       Assign source numbers
      CALL FNDSOU (IDISK, ICNO, LSOURC, BUFFER, NSSOUR(NSENTR),
     *   WNTSRC, ISSOUR(1,NSENTR), IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1320) IERR
         IRET = 2
         GO TO 990
         END IF
C                                       Freq. id.
      LKEY = 'FREQID'
      J = IKEY (LKEY, LPARS, NPARS)
      ISFQD(NSENTR) = 0
      IF (DVALS(J).NE.DBLANK) ISFQD(NSENTR) = DVALS(J)
C                                       Time range
      LKEY = 'TIMERANG'
      J = IKEY (LKEY, LPARS, NPARS)
      DO 520 M = 1, 2
         CALL RFILL (4, 0.0, TIME)
         DO 500 I = 1, 4
            K = J + I - 1
            IF (DVALS(K).NE.DBLANK) TIME(I) = DVALS(K)
500         CONTINUE
         TDAY = TIME(1) + TIME(2) / 24.0 + TIME(3) / 1440.0 +
     *      TIME(4) / 86400.0
         IF (M.EQ.1) STIMEA(M) = TDAY
         IF (M.EQ.2) STIMEB(M) = TDAY
520      CONTINUE
C                                       New subarray number
      LKEY = 'SUBARRAY'
      J = IKEY (LKEY, LPARS, NPARS)
      ISSUBA(NSENTR) = 0
      IF (DVALS(J).NE.DBLANK) ISSUBA(NSENTR) = DVALS(J) + 0.1
C                                       Old subarray number
      LKEY = 'OLD_SUBA'
      J = IKEY (LKEY, LPARS, NPARS)
      ISOLD(NSENTR) = 0
      IF (DVALS(J).NE.DBLANK) ISOLD(NSENTR) = DVALS(J) + 0.1
C                                       Read next KEYIN record
      GO TO 100
C                                       Endwhile
900   IF (WERROR) THEN
         IRET = 9
         GO TO 990
         END IF
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Hard close before exit
999   CALL ZTXCLS (JLUN, IFINDF, IERR)
C----------------------------------------------------------------------
1050  FORMAT ('EXTSUB: ERROR',I4,' READING KEYIN RECORD')
1120  FORMAT ('EXTSUB: ERROR',I4,' RETURNED BY KEYIN')
1160  FORMAT ('EXTSUB: PARAMETER MAXENT TOO SMALL. CONTACT AIPS ADMIN')
1320  FORMAT ('EXTSUB: ERROR',I4,' RETURNED BY FNDSOU')
      END
      FUNCTION IKEY (LKEY, LPARS, NPARS)
C-----------------------------------------------------------------------
C   Routine to search array LPARS for string LKEY
C   Inputs:
C      LKEY    C*8     String to search for
C      LPARS   C*8(*)  Array to be searched
C      NPARS   I       Length of LPARS
C   Outputs:
C      IKEY    I       Index in LPARS; 0 if not found.
C-----------------------------------------------------------------------
      INTEGER IKEY, NPARS
      CHARACTER*8 LKEY, LPARS(NPARS)
C
      INCLUDE 'INCS:DMSG.INC'
      INTEGER I
C-----------------------------------------------------------------------
      I = 1
C                                       While (I < N) and (Not Found) do
20    IF ((I.GT.NPARS).OR.(LKEY.EQ.LPARS(I))) GO TO 50
         I = I + 1
         GO TO 20
C
50    IF (I.GT.NPARS) THEN
         WRITE (MSGTXT,1000) LKEY
         CALL MSGWRT (8)
         STOP
         END IF
C                                       Exit
      IKEY = I
      RETURN
C----------------------------------------------------------------------
1000  FORMAT ('FATAL ERROR: CONTACT AIPS ADMIN; LKEY=',A8)
      END
      SUBROUTINE ANINDX (IDISK, ICNO, CATBLK, BUFFER, ISANDX, NSANDX,
     *   NXTSUB, WUNIQ, IRET)
C----------------------------------------------------------------------
C   Compile a lookup index of station numbers in each subarray
C   Input:
C      IDISK    I        Disk volume number.
C      ICNO     I        Catalog number.
C      CATBLK   I(256)   Catalog header
C      BUFFER   I(512)   Table I/O buffer.
C   Output:
C      ISANDX   I(*)     Antenna lookup index.
C      NSANDX   I        Dimension of ISANDX.
C      NXTSUB   I        Next highest subarray number.
C      WUNIQ    L        True if all AN tables match.
C      IRET     I        Return code (0 => ok; else error)
C----------------------------------------------------------------------
      LOGICAL WUNIQ
      REAL BUFFER(512)
      INTEGER IDISK, ICNO, CATBLK(256), ISANDX(*), NSANDX, NXTSUB,
     *   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANS.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WFOUND
      CHARACTER LNAME(MAXANT,99)*8, LRDATE(99)*8
      DOUBLE PRECISION DRFREQ(99)
      INTEGER IANTNO(MAXANT,99), NANT(99), NMXANT, ISUBA, JSUBA,
     *   I, J, K, M, IERR, IMATCH
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
      NSANDX = 0
C                                       Find highest AN table no.
      CALL FNDEXT ('AN', CATBLK, NXTSUB)
      NXTSUB = NXTSUB + 1
C                                       Loop over subarray number
      DO 100 ISUBA = 1, (NXTSUB - 1)
C                                       Read AN table
         CALL GETANT (IDISK, ICNO, ISUBA, CATBLK, BUFFER, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            GO TO 990
            END IF
C                                       Copy to local arrays
         NANT(ISUBA) = NSTNS
         DO 75 J = 1, NSTNS
            IF (STNNAM(J).EQ.' ') WRITE (STNNAM(J),1000) ISUBA, J
            LNAME(J,ISUBA) = STNNAM(J)
            IANTNO(J,ISUBA) = TELNO(J)
75          CONTINUE
C
         LRDATE(ISUBA) = RDATE
         DRFREQ(ISUBA) = SAFREQ
100      CONTINUE
C                                       Compute union of all
C                                       existing AN tables as
C                                       subarray NXTSUB
      K = 0
      NMXANT = 0
C                                       Start with AN.1
      DO 200 J = 1, NANT(1)
         K = K + 1
         LNAME(K,NXTSUB) = LNAME(J,1)
         IANTNO(K,NXTSUB) = IANTNO(J,1)
         NMXANT = MAX (NMXANT, IANTNO(J,1))
200      CONTINUE
C                                       Loop over other AN tables
      NANT(NXTSUB) = NANT(1)
      DO 350 ISUBA = 2, (NXTSUB - 1)
         DO 325 J = 1, NANT(ISUBA)
C                                       Search existing union
            WFOUND = .FALSE.
            DO 300 K = 1, NANT(NXTSUB)
               IF (LNAME(K,NXTSUB).EQ.LNAME(J,ISUBA)) WFOUND = .TRUE.
300            CONTINUE
            IF (.NOT.WFOUND) THEN
C                                       Add to union if necessary
               NANT(NXTSUB) = NANT(NXTSUB) + 1
               I = NANT(NXTSUB)
               LNAME(I,NXTSUB) = LNAME(J,ISUBA)
               IANTNO(I,NXTSUB) = IANTNO(J,ISUBA)
C                                       Is this antenna number
C                                       already used ?
               WFOUND = .FALSE.
               DO 315 M = 1, (NANT(NXTSUB) - 1)
                  IF (IANTNO(M,NXTSUB).EQ.IANTNO(J,ISUBA))
     *               WFOUND = .TRUE.
315               CONTINUE
C                                       If so, then renumber
               IF (WFOUND) THEN
                  IANTNO(I,NXTSUB) = NMXANT + 1
                  NMXANT = NMXANT + 1
                  END IF
               END IF
325         CONTINUE
350      CONTINUE
C                                       Compile lookup table
C                                       of antenna numbers in
C                                       each subarray.
      NSANDX = 0
      DO 600 ISUBA = 1, (NXTSUB - 1)
         DO 525 I = 1, NANT(ISUBA)
            IF (IANTNO(I,ISUBA).LE.0) GO TO 525
C                                       Add to index; mark as used
            NSANDX = NSANDX + 2
            ISANDX(NSANDX-1) = ISUBA
            ISANDX(NSANDX) = I
            IANTNO(I,ISUBA) = -IANTNO(I,ISUBA)
            DO 500 JSUBA = (ISUBA + 1), NXTSUB
               DO 475 J = 1, NANT(JSUBA)
                  IF (IANTNO(J,JSUBA).LE.0) GO TO 475
C                                       Check for match
                  IF (LNAME(J,JSUBA).EQ.LNAME(I,ISUBA)) THEN
C                                       Add to index; mark as used
                     NSANDX = NSANDX + 2
                     ISANDX(NSANDX-1) = JSUBA
                     ISANDX(NSANDX) = J
                     IANTNO(J,JSUBA) = -IANTNO(J,JSUBA)
                     END IF
475               CONTINUE
500            CONTINUE
C                                       Add marker between entries
            NSANDX = NSANDX + 1
            ISANDX(NSANDX) = 999
525         CONTINUE
600      CONTINUE
C                                       Are existing AN tables
C                                       identical in respect of
C                                       antenna numbering ?
      WUNIQ = .TRUE.
      I = 2
      IMATCH = ISANDX(2)
C                                       Loop through lookup table
700   IF ((I.GT.NSANDX).OR.(.NOT.WUNIQ)) GO TO 750
         IF (ISANDX(I).NE.IMATCH) WUNIQ = .FALSE.
         I = I + 2
         IF (I.LE.NSANDX) THEN
C                                       Skip over marker
            IF (ISANDX(I-1).EQ.999) THEN
               I = I + 1
               IMATCH = ISANDX(I)
               END IF
            END IF
         GO TO 700
C                                       Now match reference freq.
C                                       and ref. date
750   CONTINUE
      DO 850 I = 2, (NXTSUB - 1)
         IF ((LRDATE(I).NE.LRDATE(1)).OR.(DRFREQ(I).NE.DRFREQ(1)))
     *      WUNIQ = .FALSE.
850      CONTINUE
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ANT ',2I2.2)
      END
      SUBROUTINE TBUPDT (JLUN1, JLUN2, IRET)
C----------------------------------------------------------------------
C   Update tables to reflect new subarray assignments
C   Input:
C      JLUN1     I     First LUN for table I/O
C      JLUN2     I     Second LUN for table I/O
C   Output:
C      IRET      I     Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER JLUN1, JLUN2, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL WTABLE, WEXIST, WFITS
      INTEGER INVER, IOUTVR, IERR
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Remove index (NX) table
C                                       if it exists
      INVER = 1
      CALL ISTAB ('NX', DISKIN, OLDCNO, INVER, JLUN1, IBUFF1, WTABLE,
     *   WEXIST, WFITS, IERR)
      IF ((IERR.EQ.0).AND.(WEXIST).AND.(WTABLE)) THEN
C                                       Delete
         CALL RMEXT (DISKIN, OLDCNO, 'NX', INVER, CATBLK, IBUFF1, IERR)
C                                       Warn user
         WRITE (MSGTXT,1050)
         CALL MSGWRT (6)
         END IF
C                                       Create new AN tables
C                                       if required
      DO 100 IOUTVR = NXTSUB, NMXSUB
C                                       Copy AN.1 for now
         INVER = 1
         CALL TABCOP ('AN', INVER, IOUTVR, JLUN1, JLUN2, DISKIN,
     *      DISKIN, OLDCNO, OLDCNO, CATBLK, IBUFF1, IBUFF2, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1100) IERR, IOUTVR
            GO TO 990
            END IF
100      CONTINUE
C                                       Update subarray column
C                                       in all associated tables
      CALL SUBTAB (DISKIN, OLDCNO, CATBLK, JLUN1, IBUFF1, IERR)
      IF (IERR.NE.0) GO TO 999
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1050  FORMAT ('Deleting index (NX) table; use INDXR to re-create')
1100  FORMAT ('TBUPDT: ERROR',I4,' CREATING AN TABLE:',I3)
      END
      SUBROUTINE SUBTAB (IDISK, ICNO, CATBLK, JLUN, JBUFF, IRET)
C----------------------------------------------------------------------
C   Update subarray col. in generic tables attached to uv-data file
C   Input:
C      IDISK   I      Disk volume number
C      ICNO    I      Catalog slot number
C      JLUN    I      LUN for table I/O
C      JBUFF   I(512) Buffer for table I/O
C   Output:
C      IRET    I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER CATBLK(256), JBUFF(512), IDISK, ICNO, JLUN, IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER NCOLMX, NSUBID, NTIMID, NANTID, NSOUID, NFQID
      PARAMETER (NSUBID = 2, NTIMID = 1, NANTID = 6)
      PARAMETER (NSOUID = 4, NFQID = 3)
      PARAMETER (NCOLMX = 6)
      LOGICAL   WTABLE, WEXIST, WFITS, T, ZEROIT
      CHARACTER LTYPE*2, LSUBID(NSUBID)*24, LTIMID(NTIMID)*24,
     *   LANTID(NANTID)*24, LSOUID(NSOUID)*24, LFQID(NFQID)*24
      DOUBLE PRECISION DREC(XBPRSZ/2)
      REAL RECR(XBPRSZ), TIME
      INTEGER ITAB, NKEY, NREC, NCOL, IVER, NVER, DATP(128,2),
     *   IERR, IKOLS(NCOLMX), ISUBA, J, IANT, ISOU, IFQID,
     *   JTIME, JANT, JSUBA, JSOU, JFQID, NROW, IROW, IRNO,
     *   IREC(XBPRSZ), ISNEW, ITYP
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DREC, RECR, IREC)
      DATA LSUBID /'ARRAY', 'SUBARRAY'/
      DATA LTIMID /'TIME'/
      DATA LANTID /'ANTENNA_NO', 'ANTENNA_NOS', 'ANTENNA',
     *   'ANTENNA NO.', 'ANTS', 'ANTENNA NO'/
      DATA LSOUID /'SOURCE_ID', 'SOURCE ID', 'ID_NO.', 'ID. NO.'/
      DATA LFQID /'FREQID', 'FREQ ID', 'FRQSEL'/
      DATA T /.TRUE./
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Loop over all table
C                                       types
      CALL FXHDEX (CATBLK)
      DO 500 ITAB = 1, KIEXTN
C                                       Extract type, version.
         CALL H2CHR (2, 1, CATBLK(KHEXT+ITAB-1), LTYPE)
         NVER = CATBLK(KIVER+ITAB-1)
C                                       Loop over all table
C                                       versions.
         DO 475 IVER = 1,NVER
C                                       Is this a valid,
C                                       existing table ?
            CALL ISTAB (LTYPE, IDISK, ICNO, IVER, JLUN, JBUFF,
     *         WTABLE, WEXIST, WFITS, IERR)
C                                       Skip if invalid
            IF ((IERR.NE.0).OR.(.NOT.WEXIST).OR.(.NOT.WTABLE))
     *         GO TO 475
C                                       Open table
            CALL TABINI ('READ', LTYPE, IDISK, ICNO, IVER, CATBLK,
     *         JLUN, NKEY, NREC, NCOL, DATP, JBUFF, IERR)
            IF ((IERR.EQ.1).OR.(IERR.EQ.3)) THEN
               IRET = 1
               WRITE (MSGTXT,1100) IERR, LTYPE, IVER
               CALL MSGWRT (7)
               GO TO 475
               END IF
C                                       Search for subarray
C                                       column
            CALL FNDCOL (NSUBID, LSUBID, 24, T, JBUFF, IKOLS, IERR)
            IF ((IERR.GE.1).AND.(IERR.LE.10)) THEN
               IRET = 2
               WRITE (MSGTXT,1150) IERR, LTYPE, IVER
               CALL MSGWRT (7)
               GO TO 470
               END IF
C                                       Match found ?
            JSUBA = 0
            DO 150 J = 1, NSUBID
               IF ((IKOLS(J).GT.0).AND.(JSUBA.EQ.0)) JSUBA = IKOLS(J)
150            CONTINUE
C                                       Skip if no subarray column
C                                       in this table
            IF (JSUBA.GT.0) THEN
C                                       Message about table update
               WRITE (MSGTXT,1200) LTYPE, IVER
               CALL MSGWRT (6)
C                                       Search for subarray selection
C                                       columns (time, antenna_no,
C                                       source_id, freq_id)
C
C                                       Time
               CALL FNDCOL (NTIMID, LTIMID, 24, T, JBUFF, IKOLS, IERR)
               IF ((IERR.GE.1) .AND. (IERR.LE.10)) THEN
                  IRET = 2
                  WRITE (MSGTXT,1150) IERR, LTYPE, IVER
                  CALL MSGWRT (7)
                  GO TO 470
                  END IF
C
               JTIME = 0
               DO 200 J = 1, NTIMID
                  IF ((IKOLS(J).GT.0).AND.(JTIME.EQ.0))
     *               JTIME = IKOLS(J)
200               CONTINUE
C                                       Antenna no.
               CALL FNDCOL (NANTID, LANTID, 24, T, JBUFF, IKOLS, IERR)
               IF ((IERR.GE.1) .AND. (IERR.LE.10)) THEN
                  IRET = 2
                  WRITE (MSGTXT,1150) IERR, LTYPE, IVER
                  CALL MSGWRT (7)
                  GO TO 470
                  END IF
C
               JANT = 0
               DO 225 J = 1, NANTID
                  IF ((IKOLS(J).GT.0).AND.(JANT.EQ.0)) JANT = IKOLS(J)
225               CONTINUE
C                                       Source id.
               CALL FNDCOL (NSOUID, LSOUID, 24, T, JBUFF, IKOLS, IERR)
               IF ((IERR.GE.1).AND.(IERR.LE.10)) THEN
                  IRET = 2
                  WRITE (MSGTXT,1150) IERR, LTYPE, IVER
                  CALL MSGWRT (7)
                  GO TO 470
                  END IF
C
               JSOU = 0
               DO 250 J = 1, NSOUID
                  IF ((IKOLS(J).GT.0).AND.(JSOU.EQ.0)) JSOU = IKOLS(J)
250               CONTINUE
C                                       Freq. id.
               CALL FNDCOL (NFQID, LFQID, 24, T, JBUFF, IKOLS, IERR)
               IF ((IERR.GE.1).AND.(IERR.LE.10)) THEN
                  IRET = 2
                  WRITE (MSGTXT,1150) IERR, LTYPE, IVER
                  CALL MSGWRT (7)
                  GO TO 470
                  END IF
C
               JFQID = 0
               DO 275 J = 1, NFQID
                  IF ((IKOLS(J).GT.0).AND.(JFQID.EQ.0))
     *               JFQID = IKOLS(J)
275               CONTINUE
C                                       Close table; open for
C                                       re-write
               CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IERR)
               CALL TABINI ('WRIT', LTYPE, IDISK, ICNO, IVER, CATBLK,
     *            JLUN, NKEY, NREC, NCOL, DATP, JBUFF, IERR)
               IF ((IERR.EQ.1).OR.(IERR.EQ.3)) THEN
                  IRET = 1
                  WRITE (MSGTXT,1100) IERR, LTYPE, IVER
                  CALL MSGWRT (7)
                  GO TO 475
                  END IF
C                                       Loop through table
               NROW = JBUFF(5)
               ZEROIT = (JTIME.EQ.0) .OR. (JANT.EQ.0) .OR. (JSOU.EQ.0)
               DO 450 IROW = 1, NROW
C                                       Read record
                  IRNO = IROW
                  CALL TABIO ('READ', 0, IRNO, RECR, JBUFF, IERR)
                  IF (IERR.NE.0) THEN
                     IRET = 3
                     WRITE (MSGTXT,1150) IERR, LTYPE, IVER
                     CALL MSGWRT (7)
                     GO TO 470
                     END IF
C                                       Extract subarray selection
C                                       parameters
                  ISUBA = IREC(DATP(JSUBA,1))
                  IF (ZEROIT) THEN
                     IREC(DATP(JSUBA,1)) = 0
                  ELSE
C                                       Real or double precision ?
                     ITYP = MOD (DATP(JTIME,2), 10)
                     IF (ITYP.EQ.1) TIME = DREC(DATP(JTIME,1))
                     IF (ITYP.EQ.2) TIME = RECR(DATP(JTIME,1))
C
                     IANT = IREC(DATP(JANT,1))
                     ISOU = IREC(DATP(JSOU,1))
                     IFQID = 0
                     IF (JFQID.GT.0) IFQID = IREC(DATP(JFQID,1))
C                                       Change subarray if
C                                       selected
                     CALL FNDSUB (TIME, IANT, ISUBA, ISOU, IFQID, ISNEW)
                     IF (ISNEW.GT.0) IREC(DATP(JSUBA,1)) = ISNEW
                     END IF
C                                       Re-write record
                  IRNO = IROW
                  CALL TABIO ('WRIT', 0, IRNO, RECR, JBUFF, IERR)
                  IF (IERR.NE.0) THEN
                     WRITE (MSGTXT,1400) IERR, LTYPE, IVER
                     CALL MSGWRT (7)
                     GO TO 470
                     END IF
450               CONTINUE
C                                       Mark table as unsorted
                  JBUFF(43) = 0
                  JBUFF(44) = 0
               END IF
C                                       Close table
 470        CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IERR)
 475        CONTINUE
 500     CONTINUE
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1100  FORMAT ('SUBTAB: ERR',I3,' OPENING ',A2,' TABLE',I4)
1150  FORMAT ('SUBTAB: ERR',I3,' READING ',A2,' TABLE',I4)
1200  FORMAT ('Updating ',A2,' table; version', I4)
1400  FORMAT ('SUBTAB: ERR',I3,' WRITING ',A2,' TABLE',I4)
      END
      SUBROUTINE PRTSUB (IRET)
C----------------------------------------------------------------------
C   Print out contents of the subarray lookup table (OPCODE='SCAN')
C   Output:
C      IRET    I     Return code (0=>ok; else error)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL SECSA, SECSB
      INTEGER I, ITA(3), ITB(3), K, ISUID, ILUN, IERR, K1, K2
      DATA ILUN /25/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Print each subarray lookup
C                                       table entry
      DO 500 I = 1, NSENTR
C                                       Convert time to printable
C                                       form
         CALL PTIME (STIMEA(I), .FALSE., ITA, SECSA)
         CALL PTIME (STIMEB(I), .FALSE., ITB, SECSB)
C                                       Source name and freqid.
C                                       Get source name
         ISUID = ISSOUR(1,I)
         CALL GETSOU (ISUID, DISKIN, OLDCNO, CATBLK, ILUN, IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1150) IERR
            GO TO 990
            END IF
C
         WRITE (MSGTXT,1200) SNAME, ISFQD(I)
         CALL MSGWRT (6)
C                                       Subarray, time
         WRITE (MSGTXT,1100) ISSUBA(I), ITA, SECSA, ITB, SECSB
         CALL MSGWRT (6)
C                                       Antennas
         K1 = 1
 20      K2 = MIN (K1+19, NSANT(I))
         IF (K2.GE.K1) THEN
            WRITE (MSGTXT,1300) (ISANT(K,I), K = K1,K2)
            CALL MSGWRT (6)
            K1 = K2 + 1
            GO TO 20
            END IF
500      CONTINUE
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1100  FORMAT ('Sub: ',I4,2X,'Start:',3I3,F8.4,2X,'End:',3I3,F8.4)
1150  FORMAT ('PRTSUB: ERROR',I4,' READING SOURCE TABLE')
1200  FORMAT ('Source:',A20,5X,'Freq. id:',I4)
1300  FORMAT ('Ant:',20I3)
      END
      SUBROUTINE USUHIS
C-----------------------------------------------------------------------
C   USUHIS updates the history file.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER HILINE*72, LTIME*20
      INTEGER   LUN1, IERR, I, IDATE(3), ITIME(3), NP, K
      LOGICAL   T
      INCLUDE 'USUBA.INC'
      INCLUDE 'DSUB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUN1 /27/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Update History.
      CALL HIINIT (3)
C                                       Open history file.
      CALL HIOPEN (LUN1, DISKIN, FCNO(NCFILE), IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 980
         END IF
C                                       Task name and time
      CALL ZDATE (IDATE)
      CALL ZTIME (ITIME)
      CALL TIMDAT (ITIME, IDATE, LTIME(13:20), LTIME(1:12))
      WRITE (HILINE,1010) TSKNAM, RLSNAM, LTIME(1:12), LTIME(13:20)
      CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Case OPCODE of
C                                       'AUTO', 'SCAN'
      IF ((LOPCOD.NE.'AUTO').AND.(LOPCOD.NE.'SCAN')) GO TO 100
         WRITE (HILINE,1020) TSKNAM, LOPCOD
         CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
         GO TO 900
C                                       other:
 100  CONTINUE
C                                       Check if input adverbs
C                                       or an external text file
C                                       was used
      CALL CHBLNK (48, 1, LINFIL, NP)
      IF (NP.NE.0) THEN
C                                       External text file
         WRITE (HILINE, 1030) TSKNAM, LINFIL
         CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
      ELSE
C                                       Input adverbs:
C                                       Time range
         CALL HITIME (STIMEA(1), STIMEB(1), LUN1, IBUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
C                                       Antennas

         DO 125 I = 1, NSANT(1), 10
            WRITE (HILINE,1125) TSKNAM, (ISANT(I+K-1,1),
     *         K = 1, MIN (10, NSANT(1)-I+1))
            CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
            IF (IERR.NE.0) GO TO 980
125         CONTINUE
C                                       Sources
         DO 140 I = 1, NSSOUR(1), 2
            WRITE (HILINE,1140) TSKNAM, (SUBSRC(I+K-1),
     *         K = 1, MIN (2, NSSOUR(1)-I+1))
            CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
            IF (IERR.NE.0) GO TO 980
140         CONTINUE
C                                       Freqid.
         WRITE (HILINE,1150) TSKNAM, ISFQD(1)
         CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
         IF (IERR.NE.0) GO TO 980

C                                       New subarray
         WRITE (HILINE,1160) TSKNAM, ISSUBA(1)
         CALL HIADD (LUN1, HILINE, IBUFF1, IERR)
         IF (IERR.NE.0) GO TO 980
         GO TO 900
         END IF
C                                       Endcase
 900  CONTINUE
C                                       Close HI file
 980  CALL HICLOS (LUN1, T, IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('USUHIS: ERROR',I3,' OPENING HISTORY FILE')
 1010 FORMAT (A6,' RELEASE: ',A8,' START TIME: ',A12,2X,A8)
 1020 FORMAT (A6,' OPCODE=',A4,' / Automatic subarraying')
 1030 FORMAT (A6,' INFILE= ',A48)
 1125 FORMAT (A6,' ANTENNAS= ',10I3)
 1140 FORMAT (A6,' SRC= ',2(A16,5X))
 1150 FORMAT (A6,' FREQID= ',I4,' / Frequency id.')
 1160 FORMAT (A6,' SUBARRAY = ',I5,' / New subarray')
      END

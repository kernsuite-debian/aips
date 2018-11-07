LOCAL INCLUDE 'WTMOD.INC'
C                                       Local include for WTMOD
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INCLUDE 'INCS:PUVD.INC'
      INTEGER  SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, ILOCWT
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(10)*64
      REAL      XSIN, XDISIN, XSOUT, XDISO, XSUBA, APARM(10), ANTWT(30),
     *   ANTSCL(64), XCENT, ANTSSS(MAXANT), ANTWTS(MAXANT),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      DOUBLE PRECISION UVSCAL
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XSUBA, APARM, ANTWT, ANTSCL, XCENT
      COMMON /WTMODP/ UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   ILOCWT, ANTWTS, ANTSSS, DIFPIX
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      INCLUDE 'INCS:DCAT.INC'
LOCAL END
      PROGRAM WTMOD
C-----------------------------------------------------------------------
C! Modify weights of a uv data file
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000-2001, 2004, 2006, 2008, 2014-2015
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
C   This task will modify the weights of uv data as specified by the
C   user.  The purpose is to adjust the weights so that data from very
C   sensitive baselines will not swamp the less sensitive baselines.
C   The maximum and minimum positive weights found will be told.  The
C   output file may be the input file.
C  Adverbs:
C   INNAME.....Input UV file name (name).      Standard defaults.
C   INCLASS....Input UV file name (class).     Standard defaults.
C   INSEQ......Input UV file name (seq. #).    0 => highest.
C   INDISK.....Disk drive # of input UV file.  0 => any.
C   OUTNAME....Output UV file name (name).     Standard defaults.
C   OUTCLASS...Output UV file name (class).    Standard defaults.
C   OUTSEQ.....Output UV file name (seq. #).   0 => highest unique
C   OUTDISK....Disk drive # of output UV file. 0 => highest with
C              space for the file.
C   APARM......Weight modification control information:
C              1 = A multiplier for the weights. 0 => 1.0
C              2 = A power to which to raise the weights;
C                  0.25 - 0.5 is a useful range for VLBI.
C                  0 = > 1.0
C              3 = The maximum acceptable value of the weight.
C                  Values higher that APARM(3) will be set to
C                  APARM(3).  0 => infinity.
C   ANTWT......Constant antenna weights that replace existing weights.
C   NOISE......Antenna weight scaling
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'WTMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'WTMOD '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL WTMDIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL WTMDUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL WTMDHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE WTMDIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   WTMDIN gets input parameters for WTMOD and creates an output file
C   if necessary.
C   Inputs:
C      PRGN   C*6   Program name
C   Output:
C      JERR   I     Error code: 0 => ok
C                      5 => catalog troubles
C                      8 => can't start
C   Commons:
C      /INPARM/ all input adverbs in order given by INPUTS file
C      /MAPHDR/ output file catalog header
C   See prologue comments in WTMOD for more details.
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, BLANK*6, UTYPE*2
      INTEGER   JERR, OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'WTMOD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 120
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
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
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
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Find weight and scale.
      IF (CATBLK(KINAX).EQ.1) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
      ELSE
         ILOCWT = -1
         END IF
C                                       center frequencies
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = (INCX-CATR(KRCRP+JLOCF))
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
      ELSE
         DIFPIX = 0.0
         UVSCAL = 1.0D0
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT(1:12), KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT(1:6), KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                        Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      CALL UVPGET (IERR)
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTMDIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('WTMDIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE WTMDUV (IRET)
C-----------------------------------------------------------------------
C   WTMDUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER OFILE*48, IFILE*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, INCX,
     *   BO, VO, NUMVIS, XCOUNT, LRECO, ILENBU, KBIND, NIOUT, NIOLIM,
     *   IBIND, I, IA1, IA2, LSUBA, ISUBA
      LOGICAL   T, F
      INCLUDE 'WTMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      ISUBA = XSUBA + 0.5
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
C                                       Open vis file for write
 10   CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
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
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
      LSUBA = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
         IF (INIO.LE.0) GO TO 200
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
            IF (ILOCB.GE.0) THEN
               IA1 = BUFF1(IPTRI+ILOCB) / 256. + 0.1
               IA2 = BUFF1(IPTRI+ILOCB) - IA1*256. + 0.1
               IF (ISUBA.GT.0) LSUBA = 1.5 + 100.0 * (BUFF1(IPTRI+ILOCB)
     *            - IA1*256 - IA2)
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               IF (ISUBA.GT.0) ISUBA = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
C                                      Call user routine.
            IF (LSUBA.EQ.ISUBA) CALL WTMDFN (NUMVIS, IA1, IA2,
     *         BUFF1(IPTRI+NRPARM), BUFF1(IPTRI), INCX, IRET)
C                                       Error (fatal)
            IF (IRET .GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       advance input pointer
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
         GO TO 100
C                                       Final call to WTMDFN.
 200  NUMVIS = -1
      CALL WTMDFN (NUMVIS, IA1, IA2, BUFF1, BUFF1, INCX, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
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
 1000 FORMAT ('WTMDUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('WTMDUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('WTMDUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('WTMDUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('WTMDUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('WTMDUV: WTMDFN ERROR',I3)
 1150 FORMAT ('WTMDUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE WTMDHI
C-----------------------------------------------------------------------
C   WTMDHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, J, NONOT, JJ
      LOGICAL   T, F, ANTWRT
      INCLUDE 'WTMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T, F /.TRUE.,.FALSE./
      DATA NONOT, NOTTYP /0,'  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       APARM  inputs
      WRITE (HILINE,2000) TSKNAM, APARM(1)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2001) TSKNAM, APARM(2)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
      WRITE (HILINE,2002) TSKNAM, APARM(3)
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Write ANTWT values into history.
      DO 100 I = 1,26,5
         ANTWRT = F
         JJ = I + 4
         DO 105 J = I,JJ
            IF (ANTWT(J).NE.0.0) ANTWRT = T
 105        CONTINUE
         IF (ANTWRT) THEN
            WRITE (HILINE,2010) TSKNAM,I,(ANTWT(J),J=I,I+4)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            END IF
 100     CONTINUE
C                                      Add any user supplied history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 15         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'WTMDHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('WTMDHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('WTMDHI: ERROR COPYING TABLES')
 2000 FORMAT (A6,' APARM(1) = ',1PE12.5,' / Multiplier ')
 2001 FORMAT (A6,' APARM(2) = ',F10.5,' / Power ')
 2002 FORMAT (A6,' APARM(3) = ',1PE12.5,' / Max. weight')
 2010 FORMAT (A6,' ANTWT(',I2,') = ',5F10.2)
      END
      SUBROUTINE WTMDFN (NUMVIS, IA1, IA2, VIS, RPARM, INCX, IRET)
C-----------------------------------------------------------------------
C   This routine modifies the weights.
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C      IA1        I    First antenna number
C      IA2        I    Second antenna number
C      RPARM(*)   I    Random parameter array which includes U,V,W etc
C                      but also any other random parameters.
C      VIS(INCX,*) R    Visibilities in order real, imaginary, weight
C                      (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                      NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON
C      APARM(10)  R    User array.
C      RA         D    Right ascension (1950) of phase center. (deg)
C      DEC        D    Declination (1950) of phase center. (deg)
C      FREQ       D    Frequency of observation (Hz)
C      NRPARM     I    # random parameters.
C      NCOR       I    # correlators
C      CATBLK(256)I    Catalog header record. See Going Aips for
C                      details.
C
C   Output:
C      RPARM      R    Modified random parameter array.  N.B.  U,V,W,
C                      time, baseline should not be modified in RPARM
C      VIS        R    Visibilities
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON
C      NUMHIS            I    # history entries (max. 10)
C      HISCRD(NUMHIS)    C*64 History records
C      CATBLK            I    Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET, IA1, IA2, INCX
      REAL      VIS(INCX,*), RPARM(*)
      INTEGER   LOOP, NUMCOR, I
      LOGICAL   DOPWR, REWT, FIRST
      REAL      WTMAX, WTMIN, TEMP, WT1, WT2, XSIGN, AVWT
      INCLUDE 'WTMOD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      SAVE REWT, NUMCOR, DOPWR, FIRST, WTMAX, WTMIN
      DATA FIRST /.TRUE./
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.LT.0) GO TO 800
      IF (FIRST) THEN
         FIRST = .FALSE.
C                                       Setup
C                                       Defaults:
         IF (ABS(APARM(1)).LT.1.0E-10) APARM(1) = 1.0
         DOPWR = ABS (APARM(2)).GT.1.0E-10
         IF (.NOT.DOPWR) APARM(2) = 1.0
         IF (APARM(3).LE.1.0E-10) APARM(3) = 1.0E25
C                                       No. correlator values
         NUMCOR = (LREC - NRPARM) / INCX
         IF (NUMCOR.LE.0) NUMCOR = 1
C                                       Init, max., min
         WTMAX = 1.0E-20
         WTMIN = 1.0E20
C
         REWT = .FALSE.
         AVWT = 0.0
         DO 50 I = 1,30
            IF (ANTWT(I).NE.0.0) REWT = .TRUE.
            AVWT = AVWT + ABS (ANTWT(I))
 50         CONTINUE
         AVWT = AVWT / 30
         I = MAXANT - 30
         CALL RCOPY (30, ANTWT, ANTWTS)
         CALL RFILL (I, AVWT, ANTWTS(31))
         CALL RFILL (MAXANT, 1.0, ANTSSS)
         DO 55 I = 1,64
            IF (ANTSCL(I).GT.0.0) ANTSSS(I) = ANTSCL(I)
 55         CONTINUE
         END IF
C
      IF (REWT) THEN
         IF (ANTWTS(IA1).EQ.0.0) THEN
            WRITE (MSGTXT,1100) IA1
            IRET = 1
            GO TO 999
            END IF
         IF (ANTWTS(IA2).EQ.0.0) THEN
            WRITE (MSGTXT,1100) IA2
            IRET = 1
            GO TO 999
            END IF
         WT1 = ANTWTS(IA1)
         WT2 = ANTWTS(IA2)
         XSIGN = 1.0
         IF ((WT1.LT.0.0) .OR. (WT2.LT.0.0)) THEN
            XSIGN = -1.0
            WT1 = ABS(WT1)
            WT2 = ABS(WT2)
            END IF
         IF (ILOCWT.GE.0) THEN
            TEMP = SQRT (WT1*WT2)
            WTMAX = MAX (TEMP, WTMAX)
            WTMIN = MIN (TEMP, WTMIN)
            IF (RPARM(1+ILOCWT).LE.0.0) THEN
               RPARM(1+ILOCWT) = -TEMP
            ELSE
               RPARM(1+ILOCWT) = XSIGN * TEMP
               END IF
         ELSE
            DO 120 LOOP = 1,NUMCOR
               TEMP = SQRT (WT1*WT2)
               WTMAX = MAX (TEMP, WTMAX)
               WTMIN = MIN (TEMP, WTMIN)
               IF (VIS(3,LOOP).LE.0.0) THEN
                  VIS(3,LOOP) = -TEMP
               ELSE
                  VIS(3,LOOP) = XSIGN * TEMP
                  END IF
 120           CONTINUE
            END IF
C                                       Process record
      ELSE
         IF (ILOCWT.GE.0) THEN
            TEMP = ABS (RPARM(1+ILOCWT)) * APARM(1)
            IF (DOPWR) TEMP = TEMP ** APARM(2)
            TEMP = MIN (TEMP, APARM(3))
            TEMP = TEMP * ANTSSS(IA1) * ANTSSS(IA2)
            WTMAX = MAX (TEMP, WTMAX)
            WTMIN = MIN (TEMP, WTMIN)
            IF (RPARM(1+ILOCWT).LE.0.0) THEN
               RPARM(1+ILOCWT) = -TEMP
            ELSE
               RPARM(1+ILOCWT) = TEMP
               END IF
         ELSE
            DO 150 LOOP = 1,NUMCOR
               TEMP = ABS (VIS(3,LOOP)) * APARM(1)
               IF (DOPWR) TEMP = TEMP ** APARM(2)
               TEMP = MIN (TEMP, APARM(3))
               TEMP = TEMP * ANTSSS(IA1) * ANTSSS(IA2)
               WTMAX = MAX (TEMP, WTMAX)
               WTMIN = MIN (TEMP, WTMIN)
               IF (VIS(3,LOOP).LE.0.0) THEN
                  VIS(3,LOOP) = -TEMP
               ELSE
                  VIS(3,LOOP) = TEMP
                  END IF
 150           CONTINUE
            END IF
         END IF
      GO TO 999
C                                       Tell max, min wt.
 800  WRITE (MSGTXT,1800) WTMIN, WTMAX
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('ERROR: ANTWT(',I2,') = 0.0')
 1800 FORMAT ('Min. Abs(wt) =',1PE12.5,' Max. wt. =',E12.5)
      END

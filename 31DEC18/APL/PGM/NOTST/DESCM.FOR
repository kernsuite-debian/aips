LOCAL INCLUDE 'DESCM.INC'
C                                       Local include for DESCM
      INCLUDE 'INCS:ZPBUFSZ.INC'
      INTEGER   SEQIN, SEQ2, SEQOUT, DISKIN, DISK2, DISKO, NUMHIS,
     *   JBUFSZ
      REAL      XSIN, XDISIN, XSOUT, XDISO, XBITER, XNIT, XINC,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER HISCRD(10)*64, NAMEIN*12, CLAIN*12, NAMOUT*12, CLAOUT*6
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XBITER, XNIT, XINC, SEQIN, SEQ2, SEQOUT, DISKIN,
     *   DISK2, DISKO, NUMHIS
      COMMON /CHRCOM/ HISCRD, NAMEIN, CLAIN, NAMOUT, CLAOUT
LOCAL END
      PROGRAM DESCM
C-----------------------------------------------------------------------
C! Descimate a uv data file
C# UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2008, 2015
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
C   DESCM descimates a uv data set, copies every XINC th point to
C   the output file.
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
C      BITER          XBITER        First visibility to copy.
C      NITER          XNIT          Last visibility to consider.
C      XINC           XINC          Copy every XINC th point.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'DESCM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA PRGM /'DESCM '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL DESCIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Descimate
      CALL DESCUV (IRET)
      IF (IRET.EQ.0) CALL DESCHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DESCIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   DESCIN gets input parameters for DESCM and creates an output file
C   if necessary.
C   Inputs:  PRGN    C*6       Program name
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in DESCM for more details.
C-----------------------------------------------------------------------
      CHARACTER   STAT*4, PRGN*6, BLANK*6, UTYPE*2
      INTEGER   JERR, IROUND, NPARM, IERR, BITER, NITER, I4TEMP, OLDCNO
      LOGICAL   T
      INCLUDE 'DESCM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK /' '/
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
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Convert integers
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
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults
      IF (XBITER.LE.0.0) XBITER = 1.0
      IF (XNIT.LE.0.0) XNIT = NVIS
      IF (XINC.LE.0.0) XINC = 1.0
C                                       Change file size
      BITER = XBITER + 0.1
      NITER = XNIT + 0.1
      I4TEMP = (NITER-BITER+1) / XINC + 1000
      IF (I4TEMP.LT.CATBLK(KIGCN)) CATBLK(KIGCN) = I4TEMP
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         ELSE IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
C                                       Recover existing CATBLK
         ELSE
            FRW(NCFILE+1) = 2
            CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1065) IERR
               CALL MSGWRT (6)
               END IF
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
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DESCIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('DESCIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE DESCUV (IRET)
C-----------------------------------------------------------------------
C   DESCUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48
      INTEGER   IRET, INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO,
     *   LRECO, ILENBU, KBIND, NIOUT, NIOLIM, IBIND, I, VO, BO, NUMVIS,
     *   XCOUNT, NITER, BITER, NUMBER, VOI
      LOGICAL   T, F
      INCLUDE 'DESCM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, PHNAME, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, PHNAME, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, PHNAME, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
C                                       LRECO = length of output rec.
      LRECO = LREC
      ILENBU = 0
      NITER = XNIT + 0.1
      BITER = XBITER + 0.1
      NUMBER = NITER - BITER + 1
      CALL UVINIT ('WRIT', LUNO, INDO, NUMBER, VO, LRECO, ILENBU,
     *   JBUFSZ, BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      VOI = BITER - 1
      CALL UVINIT ('READ', LUNI, INDI, NUMBER, VOI, LREC, ILENBU,
     *   JBUFSZ, BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
C                                       Read vis. record.
 100  CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) IRET
         GO TO 990
      ELSE IF (INIO.GT.0) THEN
         IPTRI = IBIND
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            CALL DESCDO (NUMVIS, IRET)
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1
               CALL RCOPY (LREC, BUFF1(IPTRI), BUFF2(IPTRO))
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       OK, but no output please
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
         END IF
C                                       Final call to DESCDO.
         NUMVIS = -1
         CALL DESCDO (NUMVIS, IRET)
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
 1000 FORMAT ('DESCUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1010 FORMAT ('DESCUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1020 FORMAT ('DESCUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('DESCUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('DESCUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('DESCUV: DESCDO ERROR',I3)
 1150 FORMAT ('DESCUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE DESCHI
C-----------------------------------------------------------------------
C   DESCHI copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(1)*2, LABEL*8, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT, I
      LOGICAL   T
      INCLUDE 'DESCM.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /1,'NX'/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       Write input parms.
C                                       BITER
      WRITE (HILINE,2001) TSKNAM, XBITER
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       NITER
      WRITE (HILINE,2002) TSKNAM, XNIT
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       XINC
      WRITE (HILINE,2003) TSKNAM, XINC
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 20
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1010) TSKNAM
         DO 15 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 20
 15         CONTINUE
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1019)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST', BUFF1,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DESCHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1019 FORMAT ('FUGHIS: ERROR COPYING TABLES')
 2001 FORMAT (A6,' BITER =',F15.0,' /First visibility')
 2002 FORMAT (A6,' NITER =',F15.0,' /Last visibility')
 2003 FORMAT (A6,' XINC =',F10.0,' /Vis. increment')
      END
      SUBROUTINE DESCDO (NUMVIS, IRET)
C-----------------------------------------------------------------------
C   This routine decides if a data point meets the selection criterion
C   Inputs:
C      NUMVIS     I    Visibility number, -1 => final call, no data
C                      passed but allows any operations to be completed.
C   Inputs from COMMON
C      XINC       R    Number of points to skip
C   Output:
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C   Output in COMMON
C      NUMHIS     I    # history entries (max. 10)
C      HISCRD(NUMHIS) C   History records
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IRET
C
      INTEGER   COUNT, YI, YT
      INCLUDE 'DESCM.INC'
      INCLUDE 'INCS:DMSG.INC'
      SAVE COUNT, YI
C-----------------------------------------------------------------------
C                                       First call
      IF (NUMVIS.EQ.1) THEN
         COUNT = 0
         YI = XINC + 0.01
         END IF
C                                       Subsequent calls.
      IF (NUMVIS.GT.0) THEN
         YT = NUMVIS
         IRET = -1
         IF (((YT/YI)*YI).EQ.YT) IRET = 0
         IF (IRET.EQ.0) COUNT = COUNT + 1
C                                       Last call
      ELSE
         IRET = 0
         WRITE (HISCRD(1),2000) COUNT
         NUMHIS = 1
         WRITE (MSGTXT,2000) COUNT
         CALL MSGWRT (5)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 2000 FORMAT (I10,' Vis. copied')
      END

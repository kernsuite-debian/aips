      SUBROUTINE AU8 (BRANCH)
C-----------------------------------------------------------------------
C! verbs to get or clear name adverbs, destroy extension files
C# POPS-appl catalog EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2004-2007, 2009, 2012-2013
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
C   AU8 performs some catalog operations:
C   BRANCH = 1  CLRNAME  clear name fields
C            2  GETNAME  set first name fields
C            3  GET2NAME set second name fields
C            4  GET3NAME set third name fields.
C            5  EXTDEST  destroy named extension file
C            6  CLR2NAME  clear name-2 fields
C            7  CLR3NAME  clear name-3 fields
C            8  EGETNAME  set first name fields w error adverb
C            9  GETONAME  set output fields
C           10  CLRONAME  clear output fields
C           11  CHKNAME   does named entry exist
C           12  GET4NAME  set 4th name adverbs
C           13  CLR4NAME  clear 4th name adverbs
C           14  EHEX   arg -> extended hex
C           15  REHEX  string arg -> decimal
C           16  GETPOPSN return the pops number
C           17  NAMEGET return all of INNAME etc from INNAME etc
C  CLRNAME blanks out INNAME, INCLASS, INSEQ, INTYPE.
C  GETNAME will fill in INNAME, INCLASS, INSEQ, INTYPE from a
C          catalog entry (given by # via immediate argument).
C  EXTDEST Inputs:   (through adverbs in common)
C     INNAME   C*12 the name of the cataloged file.  No defaults allowed
C     INCLASS  C*6  the class of the cataloged file.
C     INSEQ    R    the sequence number of the cataloged file.
C     INDISK   R    the disk volume number of the cataloged file.  If 0,
C                   all disk volumes are searched until a match is found
C     INEXT    C*2  the type of the extension file to destroy.
C                   No defaults allowed.
C     INVERS   R    the version number of the extension file
C                   to be destroyed.  No defaults allowed.
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER  CHTEMP*18, IFILE*48, CHVOL*6, PRGNAM*6, XNNAM*12,
     *   XNCLS*6, XNTYP*2, XNEXT*2, STAT*4, CDUM*2, CNUMB*4, ATIME*8,
     *   ADATE*12, HILINE*72
      REAL      RDUM
      INTEGER   IWBLK(256), NWPL, NLPR, IHDBLK(256), ISIZE, I, ICLUN,
     *   IE, IERR, IERR2, IMAX, IMLUN, IMOD, IMXVER, IOFFSQ, ISEQ,
     *   ISLOT, IV, IVER, IVOL, XERR, IEVOL, IBVOL, XSEQ, XVOL, ICNO,
     *   IB, IFIND, ILMAX, IUSER, IWORD, IVER1, IVER2, POTERR, IREC,
     *   INUSER, MAGIC, IDUM, INDEX, J1, J2, IT(3), ID(3), IHLUN,
     *   HIBUFF(256), CONF
      LOGICAL   DISP, NOEXCL, NOSAVE, NOMAP, NOWAIT, SAVE, IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /AIPSCR/ IHDBLK, IWBLK
      EQUIVALENCE (NOEXCL, NOSAVE, NOMAP, NOWAIT)
      DATA PRGNAM /'AU8 '/
      DATA IOFFSQ, MAGIC /4, 32000/
      DATA ICLUN, IMLUN /15, 16/
      DATA NOSAVE, SAVE /.FALSE.,.TRUE./
      DATA CHVOL /'12345O'/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.19)) GO TO 999
      POTERR = 0
C                                       init for blanking
      IF ((BRANCH.NE.11) .AND. (BRANCH.NE.17)) THEN
         XNNAM = ' '
         XNCLS = ' '
         XNTYP = ' '
         XSEQ = 0
         IVOL = 0
      ELSE
         CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INTYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVERB ('INDISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      INUSER = NLUSER
      IUSER = 0
C
      GO TO (100, 200, 200, 200, 400, 100, 100, 200, 200, 100, 500, 200,
     *   100, 600, 600, 700, 800, 200, 100), BRANCH
C-----------------------------------------------------------------------
C                                       CLRNAMEs
C                                       null values to INNAME, INCLASS.
C-----------------------------------------------------------------------
 100  IF (BRANCH.EQ.1) THEN
         CALL ADVRBS ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INTYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       CLR2NAME
      ELSE IF (BRANCH.EQ.6) THEN
         CALL ADVRBS ('IN2NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN2CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN2TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN2SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       CLR3NAME
      ELSE IF (BRANCH.EQ.7) THEN
         CALL ADVRBS ('IN3NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN3CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN3TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN3SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN3DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       CLR4NAME
      ELSE IF (BRANCH.EQ.13) THEN
         CALL ADVRBS ('IN4NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN4CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN4TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN4SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN4DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       CLR5NAME
      ELSE IF (BRANCH.EQ.19) THEN
         CALL ADVRBS ('IN5NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN5CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN5TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN5SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('IN5DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       CLRONAME
      ELSE IF (BRANCH.EQ.10) THEN
         CALL ADVRBS ('OUTNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('OUTCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('OUTSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       GETNAME
C                                       GET2NAME
C                                       GET3NAME
C                                       EGETNAME
C                                       GETONAME
C                                       set name to match cat# on stack
C-----------------------------------------------------------------------
 200  IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.8)) THEN
         CALL ADVERB ('INDISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
      ELSE IF (BRANCH.EQ.3) THEN
         CALL ADVERB ('IN2DISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
      ELSE IF (BRANCH.EQ.4) THEN
         CALL ADVERB ('IN3DISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
      ELSE IF (BRANCH.EQ.12) THEN
         CALL ADVERB ('IN4DISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
      ELSE IF (BRANCH.EQ.18) THEN
         CALL ADVERB ('IN5DISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
      ELSE IF (BRANCH.EQ.9) THEN
         CALL ADVERB ('OUTDISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
      IF (IBVOL.GT.NVOL) THEN
         POTERR = 31
         WRITE (MSGTXT,1200) IBVOL, NVOL
         CALL MSGWRT (6)
         GO TO 980
         END IF
      IEVOL = IBVOL
      IF (IBVOL.LE.0) IEVOL = NVOL
      IF (IBVOL.LE.0) IBVOL = 1
      IUSER = ABS (INUSER)
      IF (IUSER.EQ.0) IUSER = NLUSER
      IF (BRANCH.EQ.8) THEN
         RDUM = -1.0
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C                                       Calculate catalog entry locatn.
      POTERR = 8
      IF ((SP.LE.0) .OR. (STACK(SP).EQ.2)) GO TO 980
      ISLOT = V(SP) + .01
      SP = SP - 1
      IF (ISLOT.LE.0) GO TO 980
      XERR = 0
      DO 240 IVOL = IBVOL,IEVOL
         IF (IBVOL.NE.IEVOL) THEN
            IF (.NOT.IAMOK(IVOL,'CA')) GO TO 240
            END IF
         CALL CATOPN (IVOL, IFIND, IHDBLK, IMAX, IERR)
         POTERR = 31
         IF (IERR.NE.0) GO TO 980
C                                       Argument out of range.
         IF ((ISLOT.LE.IMAX) .AND. (ISLOT.GT.0)) GO TO 210
            XERR = XERR + 1
            GO TO 235
 210     NWPL = 10
         NLPR = 256 / NWPL
         IMOD = (ISLOT - 1) / NLPR
         IREC = 2 + IMOD
         IWORD = 1 + NWPL*(ISLOT - NLPR*IMOD - 1)
C                                       Load proper catalog record.
         CALL ZFIO ('READ', ICLUN, IFIND, IREC, IHDBLK, IERR)
         POTERR = 50
         IF (IERR.NE.0) GO TO 250
C                                      Slot cleared.
         IF ((IHDBLK(IWORD).NE.-1) .AND. ((IHDBLK(IWORD).EQ.IUSER)
     *      .OR. (INUSER.EQ.MAGIC))) GO TO 220
            IF (IHDBLK(IWORD).EQ.-1) XERR = XERR + 10
            IF (IHDBLK(IWORD).NE.-1) XERR = XERR + 100
            GO TO 235
C                                       Tell user what he got
 220     IB = BRANCH - 1
         IF (BRANCH.EQ.8) IB = 1
         IF (BRANCH.EQ.12) IB = 4
         IF (BRANCH.EQ.18) IB = 5
         IF (BRANCH.EQ.9) IB = 6
         CALL H2CHR (18, 1, IHDBLK(IWORD+5), CHTEMP)
         XNNAM = CHTEMP(1:12)
         XNCLS = CHTEMP(13:18)
         CALL H2CHR (2, 19, IHDBLK(IWORD+5), XNTYP)
         XSEQ = IHDBLK(IWORD+IOFFSQ)
         WRITE (MSGTXT,1220) CHVOL(IB:IB), IVOL, IHDBLK(IWORD), XNTYP
         CALL NAMEST (CHTEMP, XSEQ, MSGTXT(41:), IV)
         IV = IV + 40
         CALL MSGWRT (2)
C                                       set first name
         IF ((BRANCH.EQ.2) .OR. (BRANCH.EQ.8)) THEN
            CALL ADVRBS ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('INTYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('INSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       set second name
         ELSE IF (BRANCH.EQ.3) THEN
            CALL ADVRBS ('IN2NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN2CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN2TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN2SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN2DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       set third name
         ELSE IF (BRANCH.EQ.4) THEN
            CALL ADVRBS ('IN3NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN3CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN3TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN3SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN3DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       set Fourth name
         ELSE IF (BRANCH.EQ.12) THEN
            CALL ADVRBS ('IN4NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN4CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN4TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN4SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN4DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       set Fifth name
         ELSE IF (BRANCH.EQ.18) THEN
            CALL ADVRBS ('IN5NAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN5CLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN5TYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN5SEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('IN5DISK', 'I', 1, 0, IVOL, RDUM, CDUM)
C                                       set output name
         ELSE IF (BRANCH.EQ.9) THEN
            CALL ADVRBS ('OUTNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('OUTCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('OUTSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
            IF (ERRNUM.NE.0) GO TO 250
            CALL ADVRBS ('OUTDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
            END IF
         GO TO 250
C
 235     CALL ZCLOSE (ICLUN, IFIND, IERR2)
 240     CONTINUE
C                                        Blew it!
      POTERR = 101
      WRITE (MSGTXT,1240) ISLOT, IMAX, XERR
      IF (XERR.GT.9) WRITE (MSGTXT,1241) ISLOT
      IF (XERR.GT.99) WRITE (MSGTXT,1242) ISLOT, IUSER
      CALL MSGWRT (6)
      IF (BRANCH.EQ.8) THEN
         RDUM = 1.0
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         POTERR = 0
         END IF
      GO TO 980
C                                       Close catalog file.
 250  CALL ZCLOSE (ICLUN, IFIND, IERR2)
      IF ((IERR.NE.0) .OR. (ERRNUM.NE.0)) GO TO 980
C                                        Check for duplicates
      POTERR = 0
      ICNO = 1
      XVOL = 0
      IF (IEVOL.EQ.IBVOL) XVOL = IEVOL
      IF (INUSER.EQ.MAGIC) IUSER = 0
      CALL CATDIR ('SRCH', XVOL, ICNO, XNNAM, XNCLS, XSEQ, XNTYP,
     *   IUSER, STAT, IWBLK, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1250) IERR
         CALL MSGWRT (6)
         POTERR = 101
C                                        Something is rotten
      ELSE IF ((XVOL.GT.IVOL) .OR. (ICNO.NE.ISLOT)) THEN
         WRITE (MSGTXT,1265) IVOL, ISLOT, XVOL, ICNO
         CALL MSGWRT (6)
         POTERR = 101
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       EXTDEST
C                                       destroy spec. extension file
C-----------------------------------------------------------------------
C                                       All messages handled locally.
 400  POTERR = 101
      IMXVER = 0
C                                       Convert values to integer.
      CALL ADVERB ('INSEQ', 'I', 1, 0, ISEQ, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INDISK', 'I', 1, 0, IVOL, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INVERS', 'I', 1, 0, IVER, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       namein, version no default
      CALL ADVERB ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('INEXT', 'C', 1, 2, IDUM, RDUM, XNEXT)
      IF (ERRNUM.NE.0) GO TO 980
      IF (XNNAM.EQ.' ') GO TO 950
      IF (IVER.LE.-2) GO TO 950
      IF (IVER.GT.46655) GO TO 950
      IF (XNEXT.EQ.' ') GO TO 950
      CALL ADVERB ('DOCONFRM', 'I', 1, 0, CONF, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Open map file.
      XNTYP = ' '
      DISP = NOSAVE
      CALL MAPOPN ('WRIT', IVOL, XNNAM, XNCLS, ISEQ, XNTYP, NLUSER,
     *   IMLUN, IFIND, ISLOT, CATBLK, IWBLK, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Find extension file of correct
C                                       type.
      CALL FXHDEX (CATBLK)
      IMAX = KHEXT + KIEXTN - 1
      DO 405 IE = KHEXT,IMAX
         CALL H2CHR (2, 1, CATH(IE), CDUM)
         IF (CDUM.EQ.XNEXT) THEN
            IV = KIVER + IE - KHEXT
            IMXVER = CATBLK(IV)
            END IF
 405     CONTINUE
C                                       No extension file found.
      IF (IMXVER.LE.0) THEN
         WRITE (MSGTXT,1405) XNEXT
         CALL MSGWRT (6)
C                                      Version number gt max.
      ELSE IF (IMXVER.LT.IVER) THEN
         WRITE (MSGTXT,1410) IVER, IMXVER
         CALL MSGWRT (6)
         END IF
      IF (IVER.LT.0) THEN
         IVER1 = 1
         IVER2 = MAX (1, IMXVER)
      ELSE
         IF (IVER.EQ.0) IVER = IMXVER
         IVER1 = IVER
         IVER2 = IVER
         END IF
C                                       history file
      CALL HIINIT (3)
      IHLUN = 89
      CALL HIOPEN (IHLUN, IVOL, ISLOT, HIBUFF, IERR)
      IF (IERR.NE.0) THEN
         IHLUN = 0
         IERR = 0
         END IF
      CALL ZDATE (ID)
      CALL ZTIME (IT)
      CALL TIMDAT (IT, ID, ATIME, ADATE)
C                                       Loop over files to destroy
      ILMAX = 0
      DO 420 IVER = IVER1,IVER2
C                                       Protected files
         IF ((IVER.EQ.1) .AND. ((XNEXT.EQ.'CL') .OR. (XNEXT.EQ.'HI'))
     *      .AND. (CONF.GE.-1)) THEN
            IF ((IUNIT.NE.1) .AND. (IUNIT.NE.4)) THEN
               MSGTXT = 'DELETION OF CL/HI VERS 1 NOT ALLOWED' //
     *            ' IF DOCONFRM > -2'
               CALL MSGWRT (7)
               GO TO 420
               END IF
            MSGTXT = 'You have asked to delete version 1 of a ' //
     *         XNEXT // ' file.  This is not a'
            CALL MSGWRT (4)
            MSGTXT = 'good idea usually. Do you want to do it ' //
     *         'anyway?  Enter YES or NO'
            CALL MSGWRT (4)
            POTERR = 39
            CALL CONFRM (IERR)
            IF (IERR.GT.0) GO TO 420
            END IF
C                                       Construct file name.
         CALL ZPHFIL (XNEXT, IVOL, ISLOT, IVER, IFILE, IERR)
         IF (IERR.NE.0) GO TO 450
C                                       Delete file.
         CALL ZDESTR (IVOL, IFILE, IERR)
         IF (IERR.EQ.0) THEN
            IF (IHLUN.GT.0) THEN
               WRITE (HILINE,1413) XNEXT, IVER, ADATE, ATIME
               CALL HIADD (IHLUN, HILINE, HIBUFF, IERR)
               IF (IERR.NE.0) IHLUN = 0
               IERR = 0
               END IF
            WRITE (MSGTXT,1415) XNEXT, IVER
         ELSE IF (IERR.EQ.1) THEN
            WRITE (MSGTXT,1416) XNEXT, IVER
         ELSE
            WRITE (MSGTXT,1417) XNEXT, IVER, IERR
            END IF
         CALL MSGWRT (4)
         IF (IERR.LE.1) ILMAX = IVER
         IERR = 0
 420     CONTINUE
      IF (IHLUN.GT.0) THEN
         CALL HICLOS (IHLUN, .TRUE., HIBUFF, IERR)
         IERR = 0
         END IF
      IF ((IMXVER.GT.ILMAX) .OR. (IMXVER.LE.0)) GO TO 460
      DISP = SAVE
C                                       We must reset maximum version
C                                       number in the header.
      ILMAX = IVER2
      IVER2 = IVER2 + 1
      DO 435 I = 1,ILMAX
         IVER2 = IVER2 - 1
         IF (IVER2.EQ.0) GO TO 445
C                                       Create name.
         CALL ZPHFIL (XNEXT, IVOL, ISLOT, IVER2, IFILE, IERR)
         IF (IERR.NE.0) GO TO 450
C                                       Does file exist.
         CALL ZEXIST (IVOL, IFILE, ISIZE, IERR)
C                                       0=file exists, 1=file does
C                                       not exist, 2,3 error
         IF (IERR.EQ.0) GO TO 445
         IF (IERR.NE.1) GO TO 450
 435     CONTINUE
      IVER2 = 0
      GO TO 445
C                                       Open succesful. Do close.
 445  CATBLK(IV) = IVER2
      IF (IVER2.EQ.0) CATH(IV-KIVER+KHEXT) = HBLANK
      IERR = 0
      GO TO 460
C                                       unexpected error
 450  WRITE (MSGTXT,1450) IERR
      CALL MSGWRT (7)
C                                       Close cataloged file.
 460  CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IFIND, CATBLK, DISP,
     *   IWBLK, IERR2)
      IF ((IERR.EQ.0) .AND. (IERR2.EQ.0)) GO TO 999
C                                       Cat file not closed, header
C                                       not updated.
      IF (IERR2.EQ.0) GO TO 980
         WRITE (MSGTXT,1460)
         CALL MSGWRT (8)
         GO TO 980
C-----------------------------------------------------------------------
C                                       CHKNAME
C                                       check the existence of the file
C-----------------------------------------------------------------------
 500  IF (IBVOL.LE.0) THEN
         IEVOL = NVOL
         IBVOL = 1
      ELSE
         IEVOL = IBVOL
         END IF
      IUSER = ABS (INUSER)
      IF (IUSER.EQ.0) IUSER = NLUSER
      IMAX = 0
      DO 520 IVOL = IBVOL,IEVOL
         ICNO = 0
         DO 510 INDEX = 1,1000000
            ICNO = ICNO + 1
            CALL CATDIR ('SRNN', IVOL, ICNO, XNNAM, XNCLS, XSEQ, XNTYP,
     *         IUSER, STAT, IWBLK, IERR)
            IF (IERR.EQ.0) THEN
               IMAX = IMAX + 1
            ELSE IF ((IERR.EQ.1) .OR. (IERR.EQ.5)) THEN
               GO TO 520
            ELSE
               POTERR = 50
               GO TO 980
               END IF
 510        CONTINUE
 520     CONTINUE
      RDUM = 1.0 - IMAX
      CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, RDUM, CDUM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       EHEX, REHEX conversions
C-----------------------------------------------------------------------
C                                       get immediate argument
 600  CALL ADVERB ('DENUMB', 'I', 1, 0, ISLOT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('EHNUMB', 'C', 1, 4, IDUM, RDUM, CNUMB)
      IF (ERRNUM.NE.0) GO TO 980
      IF (BRANCH.EQ.14) THEN
         CNUMB = ' '
         IF ((SP.GT.0) .AND. (STACK(SP).NE.2)) THEN
            ISLOT = V(SP) + .01
            SP = SP - 1
            END IF
         IF (ISLOT.LE.0) ISLOT = NLUSER
      ELSE
         IF ((SP.LT.4) .OR. (STACK(SP).NE.2)) THEN
            CNUMB = ' '
         ELSE IF ((STACK(SP-2).LE.8) .AND. (STACK(SP-3).EQ.14)) THEN
            CALL H2CHR (STACK(SP-2), 1, C(STACK(SP-1)), CNUMB)
            SP = SP - 4
         ELSE IF ((K(STACK(SP-2)+3).LE.8) .AND. (STACK(SP-3).EQ.7)) THEN
            CALL H2CHR (K(STACK(SP-2)+3), 1, C(STACK(SP-1)), CNUMB)
            SP = SP - 4
         ELSE
            CNUMB = ' '
            END IF
         END IF
      IF (ISLOT.LE.0) ISLOT = NLUSER
C                                       REHEX
      IF (CNUMB.NE.' ') THEN
         CALL ZHEX10 (CNUMB, ISLOT, IERR)
C                                       EHEX
      ELSE
         CALL ZEHEX (ISLOT, 4, CNUMB)
         END IF
C                                       left justify
      J2 = 1
      DO 610 J1 = 1,4
         IF ((J2.GT.1) .OR. ((CNUMB(J1:J1).NE.'0') .AND.
     *      (CNUMB(J1:J1).NE.' '))) THEN
            CNUMB(J2:J2) = CNUMB(J1:J1)
            J2 = J2 + 1
            END IF
 610     CONTINUE
      IF (J2.LE.4) CNUMB(J2:) = ' '
      WRITE (MSGTXT,1600) ISLOT, CNUMB
      CALL REFRMT (MSGTXT, '_', IMAX)
      CALL MSGWRT (4)
      CALL ADVRBS ('DENUMB', 'I', 1, 0, ISLOT, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVRBS ('EHNUMB', 'C', 1, 4, IDUM, RDUM, CNUMB)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       GETPOPSN
C-----------------------------------------------------------------------
 700  POTERR = 0
      SP = SP + 1
      V(SP) = NPOPS
      STACK(SP) = 0
      GO TO 980
C-----------------------------------------------------------------------
C                                       NAMEGET
C                                       get a matching file name
C-----------------------------------------------------------------------
 800  IUSER = ABS (INUSER)
      IF (IUSER.EQ.0) IUSER = NLUSER
      IMAX = 0
      CALL CATDIR ('SRCH', IBVOL, ICNO, XNNAM, XNCLS, XSEQ, XNTYP,
     *   IUSER, STAT, IWBLK, IERR)
      IF (IERR.EQ.0) THEN
         CALL ADVRBS ('INNAME', 'C', 1, 12, IDUM, RDUM, XNNAM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INCLASS', 'C', 1, 6, IDUM, RDUM, XNCLS)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INTYPE', 'C', 1, 2, IDUM, RDUM, XNTYP)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INSEQ', 'I', 1, 0, XSEQ, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('INDISK', 'I', 1, 0, IBVOL, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 980
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, -1.0, CDUM)
      ELSE
         POTERR = 50
         IF (IERR.EQ.5) POTERR = 0
         CALL ADVRBS ('ERROR', 'R', 1, 0, IDUM, 1.0, CDUM)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       Defaults not allowed on destroy
 950  WRITE (MSGTXT,1950)
      CALL MSGWRT (4)
C                                       AIPS error management.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1200 FORMAT ('DISK NUMBER',I4,' EXCEEDS LEGAL LIMIT ',I2)
 1220 FORMAT ('Got(',A1,')   disk=',I2,'  user=',I4,'   type=',A2)
 1240 FORMAT ('SLOT #',I7,' EXCEEDS LIMIT',2I7)
 1241 FORMAT ('CATALOG SLOT #',I7,' DOES NOT CONTAIN AN ENTRY')
 1242 FORMAT ('CATALOG SLOT #',I7,' NOT ASSIGNED TO USER',I7)
 1250 FORMAT ('CATDIR ERROR',I7,' CHECKING THE RESULT')
 1260 FORMAT ('FORCED TO SET DISK NUMBER TO',I2,' BY DUPLICATE',
     *   ' NAMES')
 1265 FORMAT ('FOUND DISK/SLOT NUMBERS',2I4,' BUT CATDIR RETURNS',2I4)
 1405 FORMAT ('WARNING: NO EXT FILE OF TYPE ',A2,' FOUND IN HEADER')
 1410 FORMAT ('WARNING: INVER =',I5,' WHILE MAX IN HEADER IS ',I4)
 1413 FORMAT ('EXDEST INEXT=''',A2,''' VER',I5,' / deleted ',A12,A8)
 1415 FORMAT ('Extension file type ',A2,' version',I4,' deleted')
 1416 FORMAT ('Extension file type ',A2,' version',I4,
     *   ' DISK FILE MISSING')
 1417 FORMAT ('EXTENSION FILE TYPE ',A2,' VERSION',I4,' DELETE',
     *   ' ERROR',I6)
 1450 FORMAT ('AU8: UNEXPECTED ERROR!',I7)
 1460 FORMAT ('AU8: FILE NOT CLOSED. HEADER NOT UPDATED')
 1600 FORMAT ('Decimal number',I11,' is ''',A,''' in extended hex')
 1950 FORMAT ('INNAME, INVER, INEXT DEFAULTS NOT ALLOWED ON DESTROY')
      END

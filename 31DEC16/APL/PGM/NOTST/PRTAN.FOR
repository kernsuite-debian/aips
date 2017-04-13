      PROGRAM PRTAN
C-----------------------------------------------------------------------
C! Prints contents of ANtenna file
C# Calibration Utility VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2001, 2004-2010, 2012
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
C   PRTAN prints the contents of an antenna 'AN' file on the line
C   printer.
C   Inputs:
C      AIPS ADVERB    PRGM NAME            Description.
C      USERID           ID          User id number for desired file.
C      INNAME           NAME        Name of desired uv data file.
C      INCLASS          CLASS       Class of desired uv data file.
C      INSEQ            SEQ         Seq. no. of desired file.
C      INDISK           DISK        Disk number of desired file.
C      INVER            VER         Version number of ext. file
C                                   0 => most recent
C      NCOUNT           NITER       Number of records to print, 0=>all.
C      DOCRT            DOCRT       > 0 => use CRT terminal, else print
C      OUTPRINT         LPNAME      File to save printer output in
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DANT.INC'
      HOLLERITH XNAME(3), XCLASS(2), XLPNAM(12)
      CHARACTER PRGM*6, NAME*12, CLASS*6, TITL1*132, TITL2*132, UTYPE*2,
     *   LINE*132, SCRTCH*132, ATIME*8, ADATE*12, MOUNT(7)*4, SPTYPE*8,
     *   LPNAME*48, TYPOLA*2, TYPOLB*2, ANAMES(MAXANT)*8
      INTEGER   SEQ, DISK, VER, IABUF(512), LUN, FIND, LUNA, CNO, IERR,
     *   DATE(3), IT(3), LUNP, FINDP, NITER, LINES, NPARM, IRET, MANT,
     *   ID, NREC, LREC, IPAGE, IANT, I, NACROS, ILINE, LLINE, NANT, J,
     *   LOCS, KEYTYP, NUMIF, IPNT, IROUND, I4TEMP, BUFFER(256), J1, J2,
     *   VALUES(2), TYPMNT, NUMVLA, IPDREF, BUFF2(1256), LUN2, JTRIM,
     *   NANTS
      LOGICAL   RQUICK, T, F, LTYPE(7), ISAPPR, ISORI, ISXY, ALLSAM
      REAL      XSEQ, XDISK, XVER, ABUFF(128), XNIT, DOCRT, AMP1, AMP2,
     *   PH1, PH2, TYPAXO, TYPLAA, TYPLAB, PP1, PP2, PP3, PP4, PD(100),
     *   PDD, ANDIAM, ANFWHM(MAXIF), DOBTW
      DOUBLE PRECISION BUFF8(64), ANTX(MAXANT), ANTY(MAXANT), DX, DY,
     *   ANTZ(MAXANT), XS, YS, ZS, ARRLON, SINA, COSA
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INCOM/ XNAME, XCLASS, XSEQ, XDISK, XVER, XNIT, DOCRT,
     *   XLPNAM, DOBTW
      EQUIVALENCE (IABUF, ABUFF, BUFF8)
      DATA PRGM /'PRTAN '/
      DATA LUN, LUNA, LUN2 /16,29,30/
      DATA T, F /.TRUE.,.FALSE./
      DATA MOUNT /'ALAZ', 'EQUA','ORBI','EW--','NS-R','NS-L','    '/
C-----------------------------------------------------------------------
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      DO 10 I = 1,MAXANT
         ANAMES(I) = ' '
         ANTX(I) = 0.0D0
         ANTY(I) = 0.0D0
         ANTZ(I) = 0.0D0
 10      CONTINUE
C                                        Get parms.
      NPARM = 23
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IRET = 8
         RQUICK = F
         GO TO 990
         END IF
      IRET = 0
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) DOCRT = MIN (-1.0,
     *   DOCRT)
      IF (DOCRT.GT.0.0) RQUICK = F
C                                        Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IRET = 8
C                                        Crunch parameters.
      ISAPPR = F
      ISORI = F
      ISXY = F
      ID = NLUSER
      SEQ = IROUND (XSEQ)
      DISK = IROUND (XDISK)
      VER = IROUND (XVER)
      NITER = IROUND (XNIT)
      CALL H2CHR (48, 1, XLPNAM, LPNAME)
C                                        Open/find uv data file.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      UTYPE = 'UV'
      CALL MAPOPN ('READ', DISK, NAME, CLASS, SEQ, UTYPE, ID, LUN,
     *   FIND, CNO, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAME, CLASS, SEQ, 'UV', DISK, ID
         CALL MSGWRT (8)
         GO TO 990
         END IF
      NREC = 1
      LREC = 1
C                                        Init AN file.
      CALL ANTINI ('READ', IABUF, DISK, CNO, VER, CATBLK, LUNA, IANRNO,
     *   ANKOLS, ANNUMV, ARRAYC, GSTIA0, DEGPDY, SAFREQ, RDATE, POLRXY,
     *   UT1UTC, DATUTC, TIMSYS, ANAME, XYZHAN, TFRAME, NUMORB, NOPCAL,
     *   ANTNIF, ANFQID, IERR)
      IF (IERR.NE.0) GO TO 980
      IF ((ABS(ARRAYC(1)).GT.1.D0) .OR. (ABS(ARRAYC(2)).GT.1.D0) .OR.
     *   (ABS(ARRAYC(3)).GT.1.D0)) DOBTW = -1.0
C                                       Find polarization solution type
      MSGSUP = 32000
      CALL TABKEY ('READ', 'POLTYPE ', 1, IABUF, LOCS, VALUES, KEYTYP,
     *   IERR)
      MSGSUP = 0
      SPTYPE = ' '
      IF ((IERR.EQ.0) .AND. (KEYTYP.EQ.3)) CALL H2CHR (8, 1, VALUES,
     *   SPTYPE)
      ISAPPR = (SPTYPE.EQ.'APPROX') .OR. (SPTYPE.EQ.'VLBI')
      ISORI = SPTYPE.EQ.'ORI-ELP '
      ISXY = SPTYPE.EQ.'X-Y LIN '
      NUMIF = ANTNIF
C                                        Open line printer.
      IF (LPNAME.EQ.' ') DOCRT = MAX (-1.0, DOCRT)
      CALL LPOPEN (LPNAME, DOCRT, LUNP, FINDP, NACROS, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL ZTIME (IT)
C                                        Prepare titles
      IPAGE = 0
      LINES = 900
      WRITE (TITL1,1020) NAME, CLASS, SEQ, VER, DISK, ID
      CALL DATEST (RDATE, DATE)
      CALL TIMDAT (IT, DATE, ATIME, ADATE)
      SAFREQ = SAFREQ * 1.0D-6
      WRITE (TITL2,1022) ANAME, SAFREQ, ADATE
C                                       Begin header info
 30   IF (DOCRT.LE.-2.5) THEN
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, TITL1,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
      ELSE
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1030)
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1031) ARRAYC
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1032) POLRXY
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1033) DEGPDY
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1034) GSTIA0
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1035) UT1UTC, TIMSYS, DATUTC
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         LINE = 'XYZHAND = ''' // XYZHAN // '''    TFRAME = ''' //
     *      TFRAME // ''''
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       Polarization solution type.
         IF ((ISAPPR) .OR. (ISXY)) THEN
            WRITE (LINE,1036)
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         IF (ISORI) THEN
C                                       Print R-L phase offsets
            CALL PDRGET (DISK, CNO, VER, LUN2, CATBLK, NUMIF, IPDREF,
     *         PD, BUFF2, IERR)
            IF (IERR.EQ.0) THEN
               WRITE (LINE,2036) IPDREF
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, LINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               DO 35 I =1,NUMIF
                  PDD = PD(I) * 57.29577951
                  WRITE (LINE,1037) I, PDD
                  CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *               LINE, LINES, IPAGE, SCRTCH, IERR)
                  IF (IERR.NE.0) GO TO 900
 35               CONTINUE
               END IF
            END IF
         IF (ANFQID.EQ.-1) THEN
            WRITE (LINE,1038)
         ELSE IF (ANFQID.GT.0) THEN
            WRITE (LINE,1039) ANFQID
            END IF
         IF (ANFQID.GE.-1) THEN
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            END IF
         END IF
C                                       What to display
      I4TEMP = IABUF(5)
      NANT = I4TEMP
      IF (NITER.GT.0) NANT = MIN (NANT, NITER)
      IF (NANT.LE.0) GO TO 900
      MANT = 0
C                                       do not display blank MOUNT type
      LTYPE(1) = F
      LTYPE(2) = F
      LTYPE(3) = F
      LTYPE(4) = F
      LTYPE(5) = F
      LTYPE(6) = F
      LTYPE(7) = F
      TYPMNT = -1000
      ALLSAM = .TRUE.
      NANTS = 0
      XS = 0.0D0
      YS = 0.0D0
      ZS = 0.0D0
C                                       read thru all data before print
      DO 40 IANT = 1,NANT
         IANRNO = IANT
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.GT.0) GO TO 960
         IF (IERR.EQ.0) THEN
            MANT = MAX (MANT, NOSTA)
            IF (MOUNT(MNTSTA+1).NE.' ') LTYPE(1) = T
            IF (STAXOF.NE.0.0) LTYPE(1) = T
            IF ((POLAA.NE.0.0) .OR. (POLAB.NE.0.0)) LTYPE(4) = T
            IF ((POLCA(1).NE.0.0) .OR. (POLCB(1).NE.0.0)) LTYPE(5) = T
            IF ((POLCA(2).NE.0.0) .OR. (POLCB(2).NE.0.0)) LTYPE(5) = T
            IF ((POLCA(3).NE.0.0) .OR. (POLCB(3).NE.0.0)) LTYPE(5) = T
C                                       Keep track of MOUNT and Pol
C                                       On first pass record values
            IF (TYPMNT.LT.-1) THEN
               TYPMNT = MNTSTA
               TYPAXO = STAXOF
               TYPOLA = POLTYA
               TYPOLB = POLTYB
               TYPLAA = POLAA
               TYPLAB = POLAB
               ANDIAM = DIAMAN
               CALL RCOPY (ANTNIF, FWHMAN, ANFWHM)
C                                       else check if value is different
            ELSE
               IF ((TYPMNT.NE.MNTSTA) .OR. (TYPAXO.NE.STAXOF) .OR.
     *            (TYPOLA.NE.POLTYA) .OR. (TYPOLB.NE.POLTYB) .OR.
     *            (TYPLAA.NE.POLAA)  .OR. (TYPLAB.NE.POLAB) .OR.
     *            (ANDIAM.NE.DIAMAN)) ALLSAM = .FALSE.
               IF (ANDIAM.NE.0.0) LTYPE(6) = T
               IF (DIAMAN.NE.0.0) LTYPE(6) = T
               DO 37 J = 1,ANTNIF
                  IF (ANFWHM(J).NE.FWHMAN(J)) ALLSAM = .FALSE.
                  IF (ANFWHM(J).NE.0.0) LTYPE(7) = T
                  IF (FWHMAN(J).NE.0.0) LTYPE(7) = T
 37            CONTINUE
               END IF
C                                       Store antenna names
            IF (NOSTA.LE.MAXANT) THEN
               ANAMES(NOSTA)(1:8) = ANNAME(1:8)
C                                       casa naming convention
               IF ((ANAME.EQ.'EVLA') .AND. (ANNAME(4:).EQ.' ') .AND.
     *            ((ANNAME(:1).EQ.'E') .OR. (ANNAME(:1).EQ.'W') .OR.
     *            (ANNAME(:1).EQ.'N')))
     *            ANAMES(NOSTA) = 'EVLA:' // ANNAME(1:3)
C                                       old VLA convention
               IF ((ANNAME(1:3).EQ.'VLA') .OR. (ANNAME(1:4).EQ.'EVLA'))
     *            THEN
                  IF ((ANNAME(1:5).EQ.'VLA: ') .OR.
     *               (ANNAME(1:5).EQ.'VLA:_') .OR.
     *               (ANNAME(1:5).EQ.'EVLA:')) THEN
                     IF (ANNAME(8:8).EQ.' ')
     *                  ANAMES(NOSTA)(7:8) = '0' // ANNAME(7:7)
                  ELSE IF ((ANNAME(1:4).EQ.'VLA:') .OR.
     *               (ANNAME(1:4).EQ.'EVLA')) THEN
                     IF (ANNAME(7:8).EQ.' ')
     *                  ANAMES(NOSTA)(6:8) = '0' // ANNAME(6:6)
                     END IF
                  END IF
               XS = XS + STAXYZ(1)
               YS = YS + STAXYZ(2)
               ZS = ZS + STAXYZ(3)
               NANTS = NANTS + 1
               ANTX(NOSTA) = STAXYZ(1)
               ANTY(NOSTA) = STAXYZ(2)
               ANTZ(NOSTA) = STAXYZ(3)
               END IF
            END IF
C                                       For all antennas loop
 40      CONTINUE
      ILINE = 3
C                                       if values all the same, list once
      IF ((ALLSAM) .AND. (DOCRT.GT.-2.5)) THEN
         WRITE (LINE,1041) MOUNT(MNTSTA+1), STAXOF
C                                       if mount value not blank
         IF (LTYPE(1)) CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *      TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1042) POLTYA, POLTYB
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1043) POLAA, POLAB
         IF (LTYPE(4)) CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *      TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1045) ANDIAM
         IF (LTYPE(6)) CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *      TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         IF (LTYPE(7)) THEN
            J1 = 1
 41         J2 = MIN (ANTNIF, J1+5)
            WRITE (LINE,1046) (ANFWHM(J), J = J1,J2)
            IF (J2.EQ.ANTNIF) THEN
               J = JTRIM (LINE)
               LINE(J+2:) = 'degrees'
               END IF
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *         LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            J1 = J2 + 1
            IF (J1.LE.ANTNIF) GO TO 41
            END IF
C                                       value not all same
C                                       count lines per antenna
      ELSE IF (.NOT.ALLSAM) THEN
         IF (LTYPE(1)) ILINE = ILINE + 1
         IF (LTYPE(4)) ILINE = ILINE + 1
         IF (LTYPE(5)) ILINE = ILINE + 4
         IF (LTYPE(6)) ILINE = ILINE + 1
         IF (LTYPE(7)) ILINE = ILINE + (ANTNIF-1) / 6 + 1
         END IF
C                                        Loop thru file.
      DO 200 IANT = 1,NANT
         IANRNO = IANT
         CALL TABAN ('READ', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *      STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN,
     *      POLTYA, POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
         IF (IERR.GT.0) GO TO 960
         IF (IERR.LT.0) GO TO 200
C                                       Add space if not all same
         IF (((.NOT.ALLSAM) .OR. (IANT.EQ.1)) .AND. (DOCRT.GT.-2.5))
     *      THEN
C                                       Page if not hold next
            LLINE = LINES + ILINE + 1
            LINES = LINES + ILINE
            LINE = ' '
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *         TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            IF (LINES.EQ.LLINE) LINES = LINES - ILINE
            END IF
C                                       Write contents of record
         IF (NOSTA.LE.MAXANT) ANNAME = ANAMES(NOSTA)
         WRITE (LINE,1040) NOSTA, ANNAME, STAXYZ
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
C                                       IF mount, axis pol values
C                                       different
         IF (.NOT.ALLSAM) THEN
            WRITE (LINE,1041) MOUNT(MNTSTA+1), STAXOF
C                                       if mount value not blank
            IF (LTYPE(1))
     *         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *         TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            WRITE (LINE,1042) POLTYA, POLTYB
            CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *         TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            WRITE (LINE,1043) POLAA, POLAB
            IF (LTYPE(4))
     *         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1,
     *         TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            WRITE (LINE,1045) DIAMAN
            IF (LTYPE(6)) CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *         TITL1, TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 900
            IF (LTYPE(7)) THEN
               J1 = 1
 46            J2 = MIN (ANTNIF, J1+5)
               WRITE (LINE,1046) (FWHMAN(J), J = J1,J2)
               IF (J2.EQ.ANTNIF) THEN
                  J = JTRIM (LINE)
                  LINE(J+2:) = 'degrees'
                  END IF
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, LINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               J1 = J2 + 1
               IF (J1.LE.ANTNIF) GO TO 46
               END IF
            END IF
C                                       Polarization feed parameters
         IPNT = 1
         IF (ISAPPR.OR.ISXY) THEN
C                                       Linear approximation
            DO 50 I = 1,NUMIF
               AMP1 = SQRT ((POLCA(IPNT)**2) + (POLCA(IPNT+1)**2))
               PH1 = ATAN2 (POLCA(IPNT+1), POLCA(IPNT)+1.0E-20) *
     *            57.296
               AMP2 = SQRT ((POLCB(IPNT)**2) + (POLCB(IPNT+1)**2))
               PH2 = ATAN2 (POLCB(IPNT+1), POLCB(IPNT)+1.0E-20) *
     *            57.296
               WRITE (LINE,1048) I, AMP1, PH1, AMP2, PH2
               IPNT = IPNT + 2
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *            TITL1, TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
 50            CONTINUE
            END IF
         IF (ISORI) THEN
C                                       Orientation-ellipticity
            DO 60 I = 1,NUMIF
               PP1 = 57.296 * POLCA(IPNT)
               PP2 = 57.296 * POLCA(IPNT+1)
               PP3 = 57.296 * POLCB(IPNT)
               PP4 = 57.296 * POLCB(IPNT+1)
               WRITE (LINE,1049) I, PP1, PP2, PP3, PP4
               IPNT = IPNT + 2
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS,
     *            TITL1, TITL2, LINE, LINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
 60            CONTINUE
            END IF
 200     CONTINUE
C                                       display relative positions?
      IF (DOBTW.GT.0.0) THEN
C                                       from FILLM for EVLA
         IF (ANAME.EQ.'EVLA') THEN
            XS = -1601185.365D0
            YS = -5041977.547D0
            ZS =  3554875.87D0
         ELSE IF ((ANAME.EQ.'VLA') .AND. (ARRAYC(1).NE.0.0D0)) THEN
            GO TO 250
         ELSE IF (NANTS.GT.0) THEN
            XS = XS / NANTS
            YS = YS / NANTS
            ZS = ZS / NANTS
            END IF
         ARRAYC(1) = ARRAYC(1) + XS
         ARRAYC(2) = ARRAYC(2) + YS
         ARRAYC(3) = ARRAYC(3) + ZS
         ARRLON = ATAN2 (ARRAYC(2), ARRAYC(1))
         COSA = COS (ARRLON)
         SINA = SIN (ARRLON)
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         LINE(10:) = 'Relative antenna positions wrt to new center:'
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         WRITE (LINE,1031) ARRAYC
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         LINE = ' '
         CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2, LINE,
     *      LINES, IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 900
         DO 220 NOSTA = 1,MAXANT
            IF ((ABS(ANTX(NOSTA)).GT.1.D0) .OR.
     *         (ABS(ANTY(NOSTA)).GT.1.D0) .OR.
     *         (ABS(ANTZ(NOSTA)).NE.0.0D0)) THEN
               DX = ANTX(NOSTA) - XS
               DY = ANTY(NOSTA) - YS
               STAXYZ(1) = DX * COSA + DY * SINA
               STAXYZ(2) = DY * COSA - DX * SINA
               STAXYZ(3) = ANTZ(NOSTA) - ZS
               WRITE (LINE,1040) NOSTA, ANAMES(NOSTA), STAXYZ
               CALL PRTLIN (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *            LINE, LINES, IPAGE, SCRTCH, IERR)
               IF (IERR.NE.0) GO TO 900
               END IF
 220        CONTINUE
         END IF
C                                       count number of VLA antennas
 250  NUMVLA = 0
C                                       count vla antennas
      DO 300 IANT = 1,MANT
         IF ((ANAMES(IANT)(1:3).EQ.'VLA') .OR.
     *      (ANAMES(IANT)(1:4).EQ.'EVLA')) NUMVLA = NUMVLA + 1
 300     CONTINUE
C                                       if a few vla antennas
C                                       print vla antenna locations
      IF (NUMVLA.GT.2) THEN
         IANT = MIN (MAXANT, MANT)
         CALL PRTVLA (LUNP, FINDP, DOCRT, NACROS, TITL1, TITL2,
     *      LINE, LINES, IPAGE, SCRTCH, IANT, ANAMES, IERR)
         END IF
C                                        Close up.
 900  IF (IERR.LE.0) THEN
         IRET = 0
      ELSE
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (8)
         END IF
C
 960  CALL LPCLOS (LUNP, FINDP, LINES, IERR)
 970  CALL TABAN ('CLOS', IABUF, IANRNO, ANKOLS, ANNUMV, ANNAME,
     *   STAXYZ, ORBPRM, NOSTA, MNTSTA, STAXOF, DIAMAN, FWHMAN, POLTYA,
     *   POLAA, POLCA, POLTYB, POLAB, POLCB, IERR)
 980  CALL MAPCLS ('READ', DISK, CNO, LUN, FIND, CATBLK, F, BUFFER,
     *   IERR)
C
 990  CALL DIETSK (IRET, RQUICK, BUFFER)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,1X,A2,
     *   ' DISK=',I3,' USER=',I5)
 1020 FORMAT ('File=',A12,'.',A6,'.',I4,5X,'An.ver=',I4,5X,'Vol=',I2,
     *   5X,'User=',I5)
 1022 FORMAT ('Array= ',A8,5X,'Freq=',F13.6,' MHz',5X,'Ref.date= ',
     *   A12)
 1030 FORMAT ('Array reference position in meters (Earth centered)')
 1031 FORMAT ('Array BX=',F16.5,4X,'BY=',F16.5,4X,'BZ=',F16.5)
 1032 FORMAT ('Polar X =',F10.5,' Polar Y =',
     *   F10.5,' arcsec')
 1033 FORMAT ('Earth rotation rate =',F16.10,' degrees / IAT day')
 1034 FORMAT ('GST at UT=0 =',F16.10,' degrees')
 1035 FORMAT ('UT1-UTC=',F15.7,3X,'Data time(',A,')-UTC=',F15.7,
     *  ' seconds')
 1036 FORMAT ('Polarization solutions are linear approximations')
 2036 FORMAT ('Polarization solutions are ellipticity-orientation, ',
     *   'ref. ant=', I3)
 1037 FORMAT ('Right-Left phase difference of IF ',I3, ' is ', F10.3,
     *   ' degrees')
 1038 FORMAT ('Solutions not yet determined for a particular FREQID')
 1039 FORMAT ('Solutions determined for FREQID ',I5)
 1040 FORMAT ('Ant',I4,' = ',A8,' BX=',F14.4,' BY=',F14.4,' BZ=',
     *   F14.4)
 1041 FORMAT ('Mount=',A4,'  Axis offset=',F8.4,' meters    IFA',
     *   15X,'IFB')
 1042 FORMAT ('Feed polarization type =',4X,2(16X,A2))
 1043 FORMAT ('Feed position angle (deg) = ',11X,F7.2,15X,F7.2)
 1045 FORMAT ('Antenna diameter       =  ',F6.1,' meters')
 1046 FORMAT ('SD beam at lambda 1m   = ',6F7.3)
 1048 FORMAT ('Lin. approx. IF(',I3,') as amp, phase = ',
     *   F7.4,',',F7.1,3X,F7.4,',',F7.1)
 1049 FORMAT ('Ellip., orient. (deg) IF(',I3,') = ',
     *   5X,F7.2,',',F7.2,3X,F7.2,',',F7.2)
 1900 FORMAT ('PRINTER ERROR',I7)
      END
      SUBROUTINE PRTVLA (OUTLUN, OUTIND, DOCRT, NC, T1, T2, LINE, NLINE,
     *   IPAGE, SCRTCH, NANT, ANAMES, IERR)
C-----------------------------------------------------------------------
C   PRTVLA prints the location of the antennas in the VLA
C   Inputs:
C      OUTLUN  I       LUN for print device (open)
C      OUTIND  I       FTAB pointer for print device
C      DOCRT   R       > 0. => use CRT, else line printer
C      NC      I       Number characters in line
C      T1      C*132   Page title line 1
C      T2      C*132   Page title line 2
C      LINE    C*132   Text line
C      NLINE   I       Number lines so far on page
C                         > 1000 => just ask about continuing
C                         = 999 => just start new page
C      IPAGE   I       Current page number
C                         = 0 => just start new page
C      SCRTCH  C*(*)   Scratch core > 132
C      NANT    I       Number of antennas in antenna name array
C      ANAMES  C*8(*)  Array of antenna names
C   Output:
C      IERR    I       Error code: 0 => OK, -1 user asks to quit
C-----------------------------------------------------------------------
      INTEGER   OUTLUN, OUTIND, NC, NLINE, IPAGE, NANT, IERR
      REAL      DOCRT
      CHARACTER SCRTCH*(*), T1*(*), T2*(*), LINE*(*), ANAMES(*)*8
C
      INTEGER   MAXPDN
      PARAMETER (MAXPDN=24)
      INTEGER   I, ARMN(50), ARME(50), ARMW(50), N, IPAD, I1, I2, I3,
     *   NEVLA
      CHARACTER PADNAM(MAXPDN)*2, ENAM*9, WNAM*9, BLANKS*25, ERMN(50)*1,
     *   ERME(50)*1, ERMW(50)*1
      LOGICAL   AFOUND(100)
      DATA PADNAM /'01','02','03','04','05','06',
     *             '07','08','09','10','12','14',
     *             '16','18','20','24','28',
     *             '32','36','40','48','56','64','72'/
      DATA ARMN, ARME, ARMW /50*0,50*0,50*0/
      DATA BLANKS /' '/
C-----------------------------------------------------------------------
C                                       for all antennas
      NEVLA = 0
      DO 200 I = 1,NANT
C                                       assume antenna is not found
         AFOUND(I) = .FALSE.
         IF ((ANAMES(I)(:3).EQ.'VLA') .OR. (ANAMES(I)(:4).EQ.'EVLA'))
     *      THEN
C                                       new form
            IF ((ANAMES(I)(:5).EQ.'VLA: ') .OR.
     *         (ANAMES(I)(:5).EQ.'VLA:_') .OR.
     *         (ANAMES(I)(:5).EQ.'EVLA:')) THEN
               I1 = 6
               I2 = 7
               I3 = 8
            ELSE
               I1 = 5
               I2 = 6
               I3 = 7
               END IF
C                                       match name and pad number
            DO 100 IPAD = 1, MAXPDN
C                                       match number
               IF (ANAMES(I)(I2:I3).EQ.PADNAM(IPAD)) THEN
C                                       match name to arm location
                  IF ((ANAMES(I)(I1:I1).EQ.'N') .OR.
     *               (ANAMES(I)(I1:I1).EQ.'n')) THEN
                     ARMN(IPAD) = I
                     AFOUND(I) = .TRUE.
                     IF (ANAMES(I)(:4).EQ.'EVLA') THEN
                        ERMN(IPAD) = '*'
                        NEVLA = NEVLA + 1
                     ELSE
                        ERMN(IPAD) = ' '
                        END IF
                     END IF
                  IF ((ANAMES(I)(I1:I1).EQ.'E') .OR.
     *               (ANAMES(I)(I1:I1).EQ.'e')) THEN
                     ARME(IPAD) = I
                     AFOUND(I) = .TRUE.
                     IF (ANAMES(I)(:4).EQ.'EVLA') THEN
                        ERME(IPAD) = '*'
                        NEVLA = NEVLA + 1
                     ELSE
                        ERME(IPAD) = ' '
                        END IF
                     END IF
                  IF ((ANAMES(I)(I1:I1).EQ.'W') .OR.
     *               (ANAMES(I)(I1:I1).EQ.'w')) THEN
                     ARMW(IPAD) = I
                     AFOUND(I) = .TRUE.
                     IF (ANAMES(I)(:4).EQ.'EVLA') THEN
                        ERMW(IPAD) = '*'
                        NEVLA = NEVLA + 1
                     ELSE
                        ERMW(IPAD) = ' '
                        END IF
                     END IF
                  END IF
 100           CONTINUE
            END IF
 200     CONTINUE
C                                       New Page, Label output
      NLINE = 950
C                                       add three lines of label
      DO 250 I = 1,3
C                                       add a blank line
         LINE =  ' '
C                                       add a label
         IF (I.EQ.2)
     *      LINE = '                      Location of VLA antennas '
C                                       print line
         IF ((I.EQ.2) .OR. (DOCRT.GT.-2.5)) THEN
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 250     CONTINUE
C                                       for all north antenna locations
      DO 300 IPAD = MAXPDN+1,1,-1
C                                       if location contains an antenna
C                                       then print antenna location
         IF ((ARMN(IPAD).NE.0) .OR. (ARME(IPAD).NE.0) .OR.
     *      (ARMW(IPAD).NE.0)) THEN
            IF (ARMN(IPAD).NE.0) THEN
               WRITE (LINE,1000,ERR=900) 'N' // PADNAM(IPAD),
     *            ARMN(IPAD), ERMN(IPAD)
            ELSE
               WRITE (LINE,1001)
               END IF
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2, LINE, NLINE,
     *         IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 300     CONTINUE
C
      N = 1
C                                       for all east and west locations
      DO 400 IPAD = 1,25
C                                       if location contains an antenna
         IF ((ARME(IPAD).NE.0) .OR. (ARMW(IPAD).NE.0) .OR.
     *      (ARMN(IPAD).NE.0)) THEN
C                                       if east contains a value
            IF (ARME(IPAD).NE.0) THEN
               WRITE (ENAM,1010,ERR=900) 'E', PADNAM(IPAD), ARME(IPAD),
     *            ERME(IPAD)
            ELSE
               ENAM = '    (  )  '
               END IF
C                                       if west contains a value
            IF (ARMW(IPAD).NE.0) THEN
               WRITE (WNAM,1011,ERR=900) ERMW(IPAD), ARMW(IPAD), 'W',
     *            PADNAM(IPAD)
            ELSE
               WNAM = ' (  )    '
               END IF
C                                       then print antenna location
            WRITE (LINE,1100,ERR=900) BLANKS(1:26-N), WNAM, BLANKS(1:N),
     *         BLANKS(1:N), ENAM
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
C                                       add two blanks per antenna
            N = MIN (N+2, 25)
C                                       end if arm has a value
            END IF
 400     CONTINUE
C                                       list out or non-vla antennas
      DO 500 I = 1,NANT
C                                       if antenna not found, out or
C                                       non-vla
         IF (.NOT.AFOUND(I)) THEN
            WRITE (LINE,1110,ERR=900) ANAMES(I), I
            CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2,
     *         LINE, NLINE, IPAGE, SCRTCH, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 500     CONTINUE
C                                       were EVLA
      IF (NEVLA.GT.0) THEN
         LINE = '* => EVLA ANTENNA'
         CALL PRTLIN (OUTLUN, OUTIND, DOCRT, NC, T1, T2, LINE, NLINE,
     *      IPAGE, SCRTCH, IERR)
         IF (IERR.NE.0) GO TO 999
         END IF
C                                       all is ok
      GO TO 999
C                                       Write to printer error
 900  CONTINUE
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ',30X,A3,' (',I2,')',A1)
 1001 FORMAT (35X,'(',2X,')')
 1010 FORMAT (A1,A2,' (',I2,')',A1)
 1011 FORMAT (A1,'(',I2,') ',A1,A2)
 1100 FORMAT (' ',A,A9,A,A,A9)
 1110 FORMAT (' ',25X,A,' (',I2,') ')
      END


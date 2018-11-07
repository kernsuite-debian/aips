      SUBROUTINE AU5E (BRANCH)
C-----------------------------------------------------------------------
C! verbs setting, saving, and retrieving the TV OFM tables
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998-1999, 2008-2009, 2011
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
C   Verbs to do things with the TV OFM tables
C   BRANCH = 1 verb GAMMASET  interactively experiment with the gamma
C                             correction factor; saved only on user 1
C                             password.
C            2      OFMWEDGE  interactive setting of multiple wedges
C                             in a new OFM
C            3      OFMLIST  list OFM values in message file
C            4      OFMDIR   list OFMs in OFMFIL area for user &
C                            AIPSOFM area for user 001
C            5      OFMGET   get a specified OFM from OFMFIL or AIPSOFM
C                            area
C            6      OFMPUT   save current OFM in named file in OFMFIL
C            7      OFMZAP   delete a named file in OFMFIL area
C            8      OFMTWEAK interactive adjustment of OFM
C            9      OFMADJ   numeric readjustment of the OFM
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER   QUAD, NLEVS, IMLEVS, BUFFER(256), IERR, JERR, IBUT, I,
     *   IOFM(TVMOFM,3), POTERR, IG, NCH, OFMDIM, IDUM, ICOLOR
      REAL      OFM(TVMOFM,3), RPOS(2), RBUF(256), RDUM, DOWEDG, DONEW,
     *   GAMMA(3), TOFM(TVMOFM,3)
      LOGICAL   T, F, CURSON, DOGROF, TVISON
      CHARACTER PRTNAM*48, PRGNAM*6, FILNAM*48, CDUM
C
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (BUFFER, RBUF)
      COMMON /AIPSCR/ OFM, IOFM, BUFFER
      DATA PRGNAM /'AU5E '/
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      POTERR = 31
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.9)) GO TO 999
C                                        Common TV parms
      OFMDIM = TVMOFM
      POTERR = 49
      CALL ADVERB ('TVLEVS', 'I', 1, 0, NLEVS, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('COLORS', 'I', 1, 0, ICOLOR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
C                                        open TV device
      CURSON = F
      DOGROF = F
      TVISON = F
      IF ((BRANCH.NE.4) .AND. (BRANCH.NE.7)) THEN
         CALL TVOPEN (BUFFER, IERR)
         IF (IERR.NE.0) THEN
            POTERR = 101
            GO TO 990
            END IF
C                                       check TV state
         IF ((TVLIMG(1).NE.TVLIMG(2)) .OR. (TVLIMG(1).NE.TVLIMG(3)) .OR.
     *      (TVLIMG(1).NE.TVLIMG(4)) .OR. (TVSCGX.NE.0) .OR.
     *      (TVSCGY.NE.0)) THEN
            IF ((BRANCH.EQ.2) .OR. (BRANCH.GT.7)) THEN
               POTERR = 77
               GO TO 985
               END IF
            END IF
C                                       read TV OFM
         TVISON = T
         IF ((NLEVS.LT.10) .OR. (NLEVS.LE.LUTOUT)) NLEVS = LUTOUT + 1
         IMLEVS = MAX (LUTOUT+1, NLEVS)
         IF (IMLEVS.GT.OFMINP+1) IMLEVS = OFMINP + 1
         CALL OFMIO ('READ', OFMDIM, IMLEVS, F, OFM, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       get file name to be used
      IF ((BRANCH.GE.5) .AND. (BRANCH.LE.7)) THEN
C                                       get argument from stack
         IF ((SP.GE.4) .AND. (STACK(SP).EQ.2) .AND.
     *      (STACK(SP-3).EQ.14)) THEN
            NCH = STACK(SP-2)
            CALL H2CHR (NCH, 1, C(STACK(SP-1)), FILNAM)
            SP = SP - 4
C                                       get adverb instead
         ELSE
            CALL ADVERB ('OFMFILE', 'C', 1, 48, IDUM, RDUM, FILNAM)
            IF (ERRNUM.NE.0) GO TO 980
            END IF
         END IF
C                                       branch to the verb itself
      IG = MIN (3, NGRAPH)
      POTERR = 0
      IERR = 0
      GO TO (100, 200, 300, 400, 500, 600, 700, 800, 900), BRANCH
C-----------------------------------------------------------------------
C                                       GAMMASET
C                                       interactive setting of Gamma
C-----------------------------------------------------------------------
C                                       set up graphics display
 100  DOGROF = T
      CURSON = T
      CALL OFMGAM (IG, OFMDIM, OFM, RPOS, GAMMA, IOFM, POTERR)
      IF ((POTERR.EQ.0) .AND. (GAMMA(1).NE.0.0)) THEN
         GAMMA(2) = GAMMA(1)
         GAMMA(3) = GAMMA(1)
         CALL ADVRBS ('RGBGAMMA', 'R', 3, 0, IDUM, GAMMA, CDUM)
         END IF
      GO TO 980
C-----------------------------------------------------------------------
C                                       OFMCONT
C                                       OFM as colored wedges, contours
C-----------------------------------------------------------------------
C                                       set up graphics display
 200  DOGROF = T
      CURSON = T
      CALL ADVERB ('DOWEDGE', 'R', 1, 0, IDUM, DOWEDG, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DONEWTAB', 'R', 1, 0, IDUM, DONEW, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL OFMCON (DOWEDG, IG, IMLEVS, DONEW, RPOS, OFMDIM, OFM, IOFM,
     *   POTERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       OFMLIST
C                                       list current OFM values
C-----------------------------------------------------------------------
 300  CALL ADVERB ('OUTPRINT', 'C', 1, 48, IDUM, RDUM, PRTNAM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL ADVERB ('DOCRT', 'R', 1, 0, IDUM, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 980
      CALL OFMLST (RDUM, PRTNAM, OFMDIM, OFM, IOFM)
      GO TO 980
C-----------------------------------------------------------------------
C                                       OFMDIR
C                                       list AIPSOFM/OFMFIL area's files
C-----------------------------------------------------------------------
 400  CALL OFMDIR ('DIR', FILNAM, OFMDIM, OFM, POTERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       OFMGET
C                                       get named OFM to TV from AIPSOFM
C                                       or OFMFIL
C-----------------------------------------------------------------------
 500  CALL OFMDIR ('GET', FILNAM, OFMDIM, TOFM, POTERR)
      IF (POTERR.NE.0) GO TO 970
C                                       ICOLOR < 0 -> 0.0
      IF (ICOLOR.LT.0) CALL RFILL (3*TVMOFM, 0.0, OFM)
      DO 510 I = 1,3
         IF ((ABS(ICOLOR).EQ.I) .OR. (ICOLOR.EQ.0)) CALL RCOPY (TVMOFM,
     *      TOFM(1,I), OFM(1,I))
 510     CONTINUE
C                                       send to the TV
      CALL OFMIO ('WRIT', OFMDIM, IMLEVS, F, OFM, IERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       OFMSAVE
C                                       create OFM file in OFMFIL w OFM
C-----------------------------------------------------------------------
 600  CALL OFMDIR ('SAVE', FILNAM, OFMDIM, OFM, POTERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       OFMZAP
C                                       kill named OFM file from OFMFIL
C-----------------------------------------------------------------------
 700  CALL OFMDIR ('ZAP', FILNAM, OFMDIM, OFM, POTERR)
      GO TO 970
C-----------------------------------------------------------------------
C                                       OFMTWEAK
C                                       interactive OFM adjustment
C-----------------------------------------------------------------------
 800  CALL OFMMOD ('TWEAK', IG, OFMDIM, OFM, IOFM, POTERR)
      CURSON = T
      GO TO 980
C-----------------------------------------------------------------------
C                                       OFMADJ
C                                       numeric linear OFM adjustment
C-----------------------------------------------------------------------
 900  DOGROF = T
      CURSON = T
      CALL OFMMOD ('ADJUS', IG, OFMDIM, OFM, IOFM, POTERR)
      GO TO 980
C-----------------------------------------------------------------------
C                                       close down
C                                       OFMDIR error
 970  IF (POTERR.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = 'OFMDIR'
         END IF
C                                       cursor off, TV closed
 980  IF (TVISON) CALL YHOLD ('ONNN', JERR)
      IF (CURSON) CALL YCURSE ('OFFF', F, F, RPOS, QUAD, IBUT, JERR)
      IG = IG + NGRAY
      IF (DOGROF) CALL YSLECT ('OFFF', IG, 0, BUFFER, JERR)
      DOGROF = (DOGROF) .AND. (NGRAPH.GE.4)
      IG = NGRAPH + NGRAY
      IF (DOGROF) CALL YSLECT ('OFFF', IG, 0, BUFFER, JERR)
 985  IF (TVISON) CALL TVCLOS (BUFFER, JERR)
C                                       messages on TV error
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1985) IERR
         CALL MSGWRT (7)
         POTERR = 49
         END IF
C                                       POPS error management
 990  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1985 FORMAT ('TV ACTION ERROR CODE',I5,' RETURNED')
      END

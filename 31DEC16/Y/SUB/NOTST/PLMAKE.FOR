      SUBROUTINE PLMAKE (NP, RPARM, IGTYPE, IERR)
C-----------------------------------------------------------------------
C! creates & opens plot file, puts into map header, writes first record
C# plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   This routine will create and open a plot file, put it in the map
C   header and write the first record into the plot file.
C   PLMAKE assumes that the map/uv file has been marked write and will
C   change it to a read flag.  It would be nice if the defaults had
C   been filled into RPARM before this routine is called.
C   Inputs:
C      NP     I      Number of floating point words in parameter list
C      RPARM  R(NP)  AIPS parameters
C      IGTYPE I      Plot file type: 1 => misc.,  2 => CNTR,
C                     3 => GREYS,  4 => PROFL,  5 => SL2PL,  6 => PCNTR,
C                     7 => IMEAN,  8 => UVPLT,  9 => GNPLT, 10 => VBPLT,
C                    11 => PFPLn, 12 => GAPLT, 13 => PLCUB, 14 => IMVIM,
C                    15 => TAPLT, 16 => POSSM, 17 => SNPLT, 18 => KNTR,
C                    19 => UVHGM, 20 => ISPEC
C                    Use 1 unless your inputs match those of these
C                    tasks - or take a new number, but
C                    AIPSUB:AU8A will need to know about it too.
C   Output:
C      IERR   I      Error code. two digit, first digit indicates
C                    subroutine: 1: MAPOPN, 2: MADDEX, 3: ZPHFIL,
C                    4: GINIT, second digit indicates error code of
C                    that subroutine.
C-----------------------------------------------------------------------
      INTEGER   NP, IERR, IGTYPE
      REAL      RPARM(*)
C
      INTEGER   IWBLK(256), IGSIZE, IGLUN, IGFIND, I
      CHARACTER PHNAME*48
      LOGICAL   SAVE
      INCLUDE 'INCS:DPLT.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA IGSIZE /0/
      DATA SAVE /.TRUE./
C-----------------------------------------------------------------------
      IF (IGTYPE.LT.1) IGTYPE = 1
      IOFFPL = 0
      XLAST = -10000.
      YLAST = XLAST
C                                       Add plot file to header
      IVER = 0
      IF (.NOT.DOTV) THEN
         CALL MADDEX ('PL', IMSTUF(7), IMSTUF(5), CATBLK, IWBLK, SAVE,
     *      'READ', IVER, IERR)
         IF (IERR.NE.0) THEN
            IERR = IERR + 20
            GO TO 999
            END IF
         END IF
C                                       Change file common
 10   IF (NCFILE.LE.0) GO TO 20
         DO 15 I = 1,NCFILE
            IF ((FVOL(I).EQ.IMSTUF(7)) .AND. (FCNO(I).EQ.IMSTUF(5)))
     *         FRW(I) = 0
 15         CONTINUE
C                                       Make physical filename
 20   CALL ZPHFIL ('PL', IMSTUF(7), IMSTUF(5), IVER, PHNAME, IERR)
      IF (IERR.EQ.0) GO TO 30
         IERR = IERR + 30
         GO TO 999
C                                       Open plot file
 30   CALL GINIT (IMSTUF(7), IMSTUF(5), PHNAME, IGSIZE, IGTYPE, NP,
     *   RPARM, DOTV, TVCHN, GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN,
     *   IGFIND, IERR)
      IF (IERR.EQ.0) GO TO 999
         GPHIND = 0
         IERR = IERR + 40
C
 999  RETURN
      END

      SUBROUTINE UVPGET (IERR)
C-----------------------------------------------------------------------
C! determines pointers to UV data from the header
C# UV-util Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 1999, 2004-2005, 2015-2016
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
C   UVPGET determines pointers and other information from a UV CATBLK.
C   The address relative to the start of a vis record for the real part
C   for a given spectral channel (CHAN) and stokes parameter (ICOR)
C   is given by :
C      NRPARM+(CHAN-1)*INCF+IABS(ICOR-ICOR0)*INCS+(IF-1)*INCIF
C   Single dish data, i.e. randomly sampled data in the image plane, is
C   also recognized and ILOCU and ILOCV point to the longitude like and
C   latitude like random parameters.  Also a "BEAM" random parameter
C   may be substitued for the "BASELINE" random parameter.  The data
C   type present may be determined from the common variable TYPUVD.
C      The types of single dish data are recognized:
C   TYPUVD=1 => unprojected RA and Dec and
C   TYPUVD=2 => projected RA and Dec (obsolete was for SELSD/GRIDR)
C   TYPUVD=3 => unprojected relative Az-El to center Ra-Dec
C      The types of UV data are
C   TYPUVD=0 => -SIN projected u,v,w's
C   TYPUVD=-1 => -NCP projected u,v's (w === 0) East-West arrays
C   Inputs: From common /MAPHDR/
C      CATBLK   I(256)   Catalog block
C      CATH     H(256)   same as CATBLK
C      CATR     R(256)   same as CATBLK
C      CATD     D(128)   same as CATBLK
C   Output: In common /UVHDR/
C      SOURCE   C*8      Source name.
C      ILOCU    I        Offset from beginning of vis record of U
C                        or longitude for single dish format data.
C      ILOCV    I        Offset from beginning of vis record of V
C                        or longitude for single dish format data.
C      ILOCW    I        Offset from beginning of vis record of W.
C      ILOCT    I                      "                        Time
C      ILOCB    I                      "                      Baseline
C                                                             (or beam)
C      ILOCSU   I                      "                    Source id.
C      ILOCFQ   I                      "                    Freq id.
C      ILOCIT   I                      "                Integration time
C      ILOCID   I                      "                Correlation id.
C      JLOCC    I        0-rel. order in data of complex values
C      JLOCS    I        Order in data of Stokes' parameters.
C      JLOCF    I        Order in data of Frequency.
C      JLOCR    I        Order in data of RA
C      JLOCD    I        Order in data of dec.
C      JLOCIF   I        Order in data of IF.
C      INCS     I        Increment in data for stokes (see above)
C      INCF     I        Increment in data for freq. (see above)
C      INCIF    I        Increment in data for IF.
C      ICOR0    I        Stokes value of first value.
C      NRPARM   I        Number of random parameters
C      LREC     I        Length in values of a vis record.
C      NVIS     I        Number of visibilities
C      FREQ     D        Frequency (Hz)
C      RA       D        Right ascension (1950) deg.
C      DEC      D        Declination (1950) deg.
C      NCOR     I        Number of correlators (Stokes' parm.)
C      ISORT    C*2      Sort order 1st 2 char meaningful.
C      TYPUVD   I        UV data type, 0=interferometer,
C                           1=single dish unprojected,
C                           2=single dish projected RA and Dec.
C      IERR     I        Return error code: 0=>OK,
C                           1, 2, 5, 7 : not all normal rand parms
C                           2, 3, 6, 7 : not all normal axes
C                           4, 5, 6, 7 : wrong bytes/value
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      CHARACTER RTYP(12)*8, ICOOR(6)*8, PCOOR(2)*5, BEM*8, CTEST*5,
     *   CTEMP*8, XCOOR(2,3)*5
      INTEGER   ILOC(12), JLOC(6), I, J, NPRM, INDEX, NAXIS, II, NCOMP,
     *   LIMIT
      LOGICAL   EQUAL, ISSD
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (ILOC(1), ILOCU)
      EQUIVALENCE (JLOC(1), JLOCC)
      DATA RTYP /'UU-L   ', 'VV-L    ', 'WW-L    ',
     *   'TIME1   ', 'BASELINE', 'SOURCE  ','FREQSEL ','INTTIM  ',
     *   'CORR-ID ', 'SUBARRAY', 'ANTENNA1', 'ANTENNA2'/
C                                       axis types - 1st set for UV
      DATA ICOOR /'COMPLEX ', 'STOKES ', 'FREQ ', 'RA ', 'DEC ', 'IF '/
C                                       any for single dish
      DATA XCOOR /'RA','DEC','GLON','GLAT','ELON','ELAT'/
C                                       single-dish relative az,el
      DATA PCOOR /'AZ---','EL---'/
      DATA BEM /'BEAM    '/
C-----------------------------------------------------------------------
      IERR = 0
      NVIS = CATBLK(KIGCN)
      CALL H2CHR (8, 1, CATH(KHOBJ), SOURCE)
C                                       ISSD = true for single dish data
      ISSD = .FALSE.
      TYPUVD = 0
C                                       Determine rand parm. locations.
      CALL FILL (12, -1, ILOC)
      NRPARM = CATBLK(KIPCN)
      NPRM = MIN (NRPARM, KIPTPN)
      DO 30 I = 1,NPRM
         INDEX = KHPTP + 2 * (I-1)
         CALL H2CHR (8, 1, CATH(INDEX), CTEMP)
         DO 30 J = 1,12
            NCOMP = 8
            IF (J.LE.3) NCOMP = 4
            EQUAL = RTYP(J)(1:NCOMP).EQ.CTEMP(1:NCOMP)
            IF (EQUAL) THEN
               ILOC(J) = I - 1
               IF ((J.LE.2) .AND. (CTEMP(5:8).EQ.'-NCP')) TYPUVD = -1
               END IF
C                                       For SD, sub. BEAM for
C                                       BASELINE.
            IF ((J.EQ.5) .AND. (ILOC(J).LT.0)) THEN
               EQUAL = BEM.EQ.CTEMP
               IF (EQUAL) ILOC(J) = I - 1
               END IF
C                                       Single dish data trap, projected
            IF ((J.LE.2) .AND. (.NOT.EQUAL)) THEN
               CTEST = CTEMP(:5)
C                                       Single dish un projected.
               EQUAL = XCOOR(J,1)(1:4).EQ.CTEST
               IF (.NOT.EQUAL) EQUAL = XCOOR(J,2).EQ.CTEST
               IF (.NOT.EQUAL) EQUAL = XCOOR(J,3).EQ.CTEST
               IF (EQUAL) THEN
                  ILOC(J) = I - 1
                  ISSD = .TRUE.
                  TYPUVD = 1
C                                       relative az-el
               ELSE
                  EQUAL = CTEST.EQ.PCOOR(J)
                  IF (EQUAL) THEN
                     ILOC(J) = I - 1
                     ISSD = .TRUE.
                     TYPUVD = 3
                     END IF
                  END IF
               END IF
 30         CONTINUE
C                                       For single dish data set ILOCW
      IF (ISSD) ILOC(3) = 0
C                                       All there?
C                                       Check required types
      LIMIT = 4
      IF (ISSD) LIMIT = 4
      DO 35 I = 1,LIMIT
         IF (ILOC(I).LE.-1) THEN
            WRITE (MSGTXT,1030) ILOC
            CALL MSGWRT (6)
            IERR = IERR + 1
            GO TO 40
            END IF
 35      CONTINUE
      IF (.NOT.ISSD) THEN
         IF ((ILOC(5).LT.0) .AND. ((ILOC(10).LT.0) .OR. (ILOC(11).LT.0)
     *      .OR. (ILOC(12).LT.0))) THEN
            WRITE (MSGTXT,1030) ILOC
            CALL MSGWRT (6)
            IERR = IERR + 1
            END IF
         END IF
C                                       Determine order of data.
 40   CALL FILL (6, -1, JLOC)
      NAXIS = CATBLK(KIDIM)
      DO 50 I = 1,NAXIS
         INDEX = KHCTP + 2 * (I-1)
         CALL H2CHR (8, 1, CATH(INDEX), CTEMP)
         DO 45 J = 1,6
            EQUAL = ICOOR(J).EQ.CTEMP
            IF ((.NOT.EQUAL) .AND. (J.GE.4) .AND. (J.LE.5)) THEN
C                                       really should not br RA---
               EQUAL = XCOOR(J-3,1).EQ.CTEMP(:5)
C                                       single-dish forgiving
               IF (ISSD) THEN
                  IF (.NOT.EQUAL) EQUAL = XCOOR(J-3,2).EQ.CTEMP(:5)
                  IF (.NOT.EQUAL) EQUAL = XCOOR(J-3,3).EQ.CTEMP(:5)
                  END IF
               END IF
            IF (EQUAL) THEN
               JLOC(J) = I - 1
               GO TO 50
               END IF
 45         CONTINUE
 50      CONTINUE
C                                       All there?
C                                       Don't check IF.
      DO 55 I = 1,5
         IF (JLOC(I).GT.-1) GO TO 55
            WRITE (MSGTXT,1050) ICOOR(I)
            CALL MSGWRT (6)
            IERR = IERR + 2
            GO TO 60
 55      CONTINUE
C                                       Logical record length
 60   LREC = 1
      DO 65 I = 1,NAXIS
         LREC = LREC * CATBLK(KINAX+I-1)
 65      CONTINUE
      LREC = LREC + NRPARM
C                                       Other parameters
      NCOR = CATBLK(KINAX+JLOCS)
      FREQ = CATD(KDCRV+JLOCF)
      IF (FREQ.LE.0.0D0) THEN
         FREQ = 1.0D0
         MSGTXT = 'CHANGED FREQ = 0 TO 1 HZ TO AVOID BIGGER HURTS'
         CALL MSGWRT (7)
         END IF
      RA = CATD(KDCRV+JLOCR)
      DEC = CATD(KDCRV+JLOCD)
      DEQUIN = CATR(KREPO)
      CALL H2CHR (2, 1, CATBLK(KITYP), ISORT)
      IF (CATR(KRCIC+JLOCS).EQ.0.0) CATR(KRCIC+JLOCS) = SIGN (1.0D0,
     *   CATD(KDCRV+JLOCS))
      ICOR0 = CATD(KDCRV+JLOCS) + (1.0-CATR(KRCRP+JLOCS))*
     *   CATR(KRCIC+JLOCS)
C                                        Determine increments in data.
      INCS = 1
      IF (JLOCS.LE.0) GO TO 75
         DO 70 II = 1,JLOCS
            INCS = INCS * CATBLK(KINAX+II-1)
 70         CONTINUE
 75   INCF = 1
      IF (JLOCF.LE.0) GO TO 85
         DO 80 II = 1,JLOCF
            INCF = INCF * CATBLK(KINAX+II-1)
 80         CONTINUE
 85   INCIF = 0
      IF (JLOCIF.LE.0) GO TO 999
         INCIF = 1
         DO 90 II = 1,JLOCIF
            INCIF = INCIF * CATBLK(KINAX+II-1)
 90         CONTINUE
C                                       Trap 1 freq, multiple IF
         IF ((CATBLK(KINAX+JLOCF).LE.1) .AND. (INCIF.GT.0)) INCF = INCIF
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('UVPGET RANDOM PARMS MISSING:',12I3)
 1050 FORMAT ('UVPGET: Missing ',A8,' axis')
      END

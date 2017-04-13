      SUBROUTINE COORDT (ITI, ITO, LONGI, LATI, EPOK, LONGO, LATO, IERR)
C-----------------------------------------------------------------------
C! translates between celestial, galactic, and eccliptic coordinates
C# Coordinates
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
C   COORDT translates between types of coordinates:
C   Inputs: ITI    I     Input type (1 Ra, Dec; 2 gal, 3 ecliptic)
C           ITO    I     Output type
C           LONGI  D     Input longitude
C           LATI   D     Input latitude
C           EPOK   R     Epoch of positions (used very simply with
C                        ecliptic coords only)
C                        1950 or 2000 assumed in Galactic conversions!!!
C   Output: LONGO  D     Output longitude
C           LATI   D     Output latitude
C           IERR   I     error: 0 ok, 1 input error, 2 conversion fails
C-----------------------------------------------------------------------
      INTEGER   ITI, ITO, IERR
      REAL      EPOK
      DOUBLE PRECISION    LONGI, LATI, LONGO, LATO
C                                           define PI/180 and 2PI
      DOUBLE PRECISION DEGRAD, CIRC
      PARAMETER (DEGRAD=1.745329251994329576D-2, 
     *   CIRC = 6.2831853071795865788D0)
      DOUBLE PRECISION RAGP, DECGP, LONCP, DEC, RA, DT,
     *   GLAT, GLON, ELAT, ELON, RAGP0, DECGP0, LONCP0, DEPS0(4), EPS
      DATA DEPS0 /23.452294D0, -0.0130125D0, -1.64D-6, 5.03D-7/
C-----------------------------------------------------------------------
      IERR = 1
      IF ((ITI.LT.1) .OR. (ITI.GT.3)) GO TO 999
      IF ((ITO.LT.1) .OR. (ITO.GT.3)) GO TO 999
      IERR = 0
C                                       null cases
      LONGO = LONGI
      LATO = LATI
      IF (ITI.EQ.ITO) GO TO 999
      DT = 0.0D0
      IF ((ITI.EQ.3) .OR. (ITO.EQ.3)) DT = (EPOK - 1900.0) / 100.0
      EPS = DEPS0(1) + DT * (DEPS0(2) + DT*(DEPS0(3) + DT*DEPS0(4)))
      EPS = EPS * DEGRAD
C                                       Get galactic coords for Epoch
      CALL GALPOL( EPOK, RAGP0, DECGP0, LONCP0)
      RAGP  =  RAGP0 * DEGRAD
      DECGP =  DECGP0 * DEGRAD
      LONCP = LONCP0 * DEGRAD
      GO TO (100, 200, 300), ITI
C                                       Ra, Dec in
 100  DEC = LATI * DEGRAD
      RA = LONGI * DEGRAD
C                                       Galactic out
 110  IF (ITO.EQ.3) GO TO 150
         GLAT = ASIN (SIN(DEC)*SIN(DECGP) + COS(DEC)*COS(DECGP)*
     *      COS(RA-RAGP))
         GLON = LONCP + ATAN2 (COS(DEC)*SIN(RAGP-RA), SIN(DEC)*
     *      COS(DECGP) - COS(DEC)*SIN(DECGP)*COS(RA-RAGP))
         IF (GLON.GE.CIRC) GLON = GLON - CIRC
         IF (GLON.LT.0.0D0) GLON = GLON + CIRC
         LATO = GLAT / DEGRAD
         LONGO = GLON / DEGRAD
         GO TO 999
C                                       Ecliptic out
 150  CONTINUE
         ELAT = ASIN (SIN(DEC)*COS(EPS) - COS(DEC)*SIN(EPS)*
     *      SIN(RA))
         ELON = ATAN2 (SIN(DEC)*SIN(EPS) -
     *      COS(DEC)*COS(EPS)*SIN(RA), COS(DEC)*COS(RA))
         IF (ELON.GE.CIRC) ELON = ELON - CIRC
         IF (ELON.LT.0.0D0) ELON = ELON + CIRC
         LATO = ELAT / DEGRAD
         LONGO = ELON / DEGRAD
         GO TO 999
C                                       Galactic in
 200  GLAT = LATI * DEGRAD
      GLON = LONGI * DEGRAD
      DEC = ASIN (SIN(GLAT)*SIN(DECGP) + COS(GLAT)*COS(DECGP)*
     *   COS(GLON-LONCP))
      RA = RAGP + ATAN2 (COS(GLAT)*SIN(LONCP-GLON), SIN(GLAT)*
     *   COS(DECGP) - COS(GLAT)*SIN(DECGP)*COS(GLON-LONCP))
C                                       Ra, Dec out
 210  IF (ITO.EQ.3) GO TO 150
         IF (RA.GE.CIRC) RA = RA - CIRC
         IF (RA.LT.0.0D0) RA = RA + CIRC
         LONGO = RA / DEGRAD
         LATO = DEC / DEGRAD
         GO TO 999
C                                       Ecliptic in
 300  ELAT = LATI * DEGRAD
      ELON = LONGI * DEGRAD
      DEC = ASIN (SIN(ELAT)*COS(EPS) +
     *   COS(ELAT)*SIN(EPS)*SIN(ELON))
      RA = ATAN2 (COS(ELAT)*COS(EPS)*SIN(ELON) -
     *   SIN(ELAT)*SIN(EPS), COS(ELAT)*COS(ELON))
C                                       Ra, Dec out
      IF (ITO.EQ.2) GO TO 110
      GO TO 210
C
 999  RETURN
      END

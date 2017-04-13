      SUBROUTINE SDDPTS (IBYTFL, HEADER)
C-----------------------------------------------------------------------
C! Translates SDD format pointers to current actual pointers SDD header
C# Singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 1999, 2005
C;  Associated Universities, Inc. Washington DC, USA.
C;  Eric W. Greisen
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
C   SDDPTS translates the header pointers to fill out the common of
C   current pointers.  Note that we assume that these remain constant
C   during any run
C   Input:
C      IBYTFL   I       Machine's real bytflp parameter
C      HEADER   D(*)    Header - 32 bytes in non-local format (16 I*2)
C   Output common /SDPTRS/ from SDD.INC
C-----------------------------------------------------------------------
      INTEGER   IBYTFL
      DOUBLE PRECISION HEADER(*)
C
      INTEGER   BUFF(16), HDP(15), I
      DOUBLE PRECISION DBUF(4)
C                                       SDD classes
      INTEGER   CDLEN, CSCAN, COBSI, CTELE, COBJE, COBSM, CFRNT, CUTDA,
     *   CSAMP, CEPOC, CXSOU, CYSOU, COFFS, CVELD, CGAIN, COBSF, CRSTF,
     *   CFRQR, CTSYS, CREFP, CX0,   CINTT, CNOIN, CFRAM, CPBEM, CMBEM,
     *   CETAA, CETAF, CRTSY, CAPPE
      PARAMETER (CDLEN = 1, CSCAN = 1, COBSI = 1, CTELE = 1, COBJE = 1,
     *   COBSM = 1, CFRNT = 1, CUTDA = 3, CSAMP = 3, CEPOC = 4,
     *   CXSOU = 4, CYSOU = 4, COFFS = 7, CVELD = 7, CGAIN = 9,
     *   COBSF = 12, CRSTF = 12, CFRQR = 12, CTSYS = 12, CREFP = 12,
     *   CX0   = 12, CINTT = 12, CNOIN = 12, CFRAM = 6,  CPBEM = 9,
     *   CMBEM = 9, CETAA = 8, CETAF = 8, CRTSY = 12, CAPPE = 8)
C                                       SDD offset within class
      INTEGER   IDLEN, ISCAN, IOBSI, ITELE, IOBJE, IOBSM, IFRNT, IUTDA,
     *   ISAMP, IEPOC, IXSOU, IYSOU, IOFFS, IVELD, IGAIN, IOBSF, IRSTF,
     *   IFRQR, ITSYS, IREFP, IX0,   IINTT, INOIN, IFRAM, IPBEM, IMBEM,
     *   IETAA, IETAF, IRTSY, IAPPE
      PARAMETER (IDLEN = 1, ISCAN = 2, IOBSI = 3, ITELE = 6, IOBJE = 8,
     *   IOBSM = 10, IFRNT = 11, IUTDA = 0, ISAMP = 7, IEPOC = 0,
     *   IXSOU = 1, IYSOU = 2, IOFFS = 1, IVELD = 5, IGAIN = 13,
     *   IOBSF = 0, IRSTF = 1, IFRQR = 2, ITSYS = 6, IREFP = 10,
     *   IX0   = 11, IINTT = 13, INOIN = 14, IFRAM = 10, IPBEM = 14,
     *   IMBEM = 16, IETAA = 3, IETAF = 4, IRTSY = 7, IAPPE = 0)
      INCLUDE 'INCS:DSDD.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (HDP(1), BUFF(2))
C-----------------------------------------------------------------------
      DBUF(1) = HEADER(1)
      DBUF(2) = HEADER(2)
      DBUF(3) = HEADER(3)
      DBUF(4) = HEADER(4)
C                                       old data written by solaris
      IF (IBYTFL.EQ.BYTFLP) THEN
         CALL ZI16IL (16, 1, DBUF, BUFF)
C                                       data written by linux
      ELSE IF (IBYTFL.EQ.0) THEN
         CALL ZIPAC2 (16, 1, BYTFLP, 1, DBUF, BUFF)
      ELSE
         CALL ZIPAC2 (16, 2, BYTFLP, 1, DBUF, BUFF)
         END IF
      DO 10 I = 1,16
         IF (BUFF(I).LT.0) BUFF(I) = BUFF(I) + 65536
 10      CONTINUE
C                                       SDD data length bytes
      SDLEN = HDP(CDLEN) + IDLEN
C                                       SDD scan number
      SSCAN = HDP(CSCAN) + ISCAN
C                                       SDD observer initials
      SOBSI = HDP(COBSI) + IOBSI
C                                       SDD telescope
      STELE = HDP(CTELE) + ITELE
C                                       SDD source name
      SOBJE = HDP(COBJE) + IOBJE
C                                       SDD observing mode
      SOBSM = HDP(COBSM) + IOBSM
C                                       SDD frontend
      SFRNT = HDP(CFRNT) + IFRNT
C                                       SDD UT date YYYY.MMDD
      SUTDA = HDP(CUTDA) + IUTDA
C                                       SDD sample rate (sec)
      SSAMP = HDP(CSAMP) + ISAMP
C                                       SDD epoch
      SEPOC = HDP(CEPOC) + IEPOC
C                                       SDD commanded position X
      SXSOU = HDP(CXSOU) + IXSOU
C                                       SDD commanded position Y
      SYSOU = HDP(CYSOU) + IYSOU
C                                       SDD off scan number
      SOFFS = HDP(COFFS) + IOFFS
C                                       SDD velocity definition
      SVELD = HDP(CVELD) + IVELD
C                                       SDD gain scan number
      SGAIN = HDP(CGAIN) + IGAIN
C                                       SDD observed frequency
      SOBSF = HDP(COBSF) + IOBSF
C                                       SDD rest frequency
      SRSTF = HDP(CRSTF) + IRSTF
C                                       SDD frequency increment
      SFRQR = HDP(CFRQR) + IFRQR
C                                       SDD system temperature
      STSYS = HDP(CTSYS) + ITSYS
C                                       SDD velocity ref pixel
      SREFP = HDP(CREFP) + IREFP
C                                       SDD velocity
      SX0   = HDP(CX0  ) + IX0
C                                       SDD total integration time
      SINTT = HDP(CINTT) + IINTT
C                                       SDD number integrations
      SNOIN = HDP(CNOIN) + INOIN
C                                       SDD coordinate frame
      SFRAM = HDP(CFRAM) + IFRAM
C                                       SDD plus beam throw
      SPBEM = HDP(CPBEM) + IPBEM
C                                       SDD minus beam throw
      SMBEM = HDP(CMBEM) + IMBEM
C                                       SDD rear spill, scatter eff.
      SETAA = HDP(CETAA) + IETAA
C                                       SDD forward spill & scatter
      SETAF = HDP(CETAF) + IETAF
C                                       SDD reference system temperature
      SRTSY = HDP(CRTSY) + IRTSY
C                                       SDD antenna aperture efficiency
      SAPPE = HDP(CAPPE) + IAPPE
C
 999  RETURN
      END

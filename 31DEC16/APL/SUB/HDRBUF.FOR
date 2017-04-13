      SUBROUTINE HDRBUF (IDIR, CATBLK, CATH, CATR, CATD, BUF)
C-----------------------------------------------------------------------
C! translates AIPS header to/from FITS-standard integer form
C# Catalog TV-util
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
C   HDRBUF translates between an AIPS standard header and a local
C   integer form of it suitable for ZILI16.  Used for inter-CPU
C   communications in the image catalog.  Thus, the header is assumed to
C   be in image catalog form.
C   Inputs:
C      IDIR   I         1 : CATn -> BUF; 2 => BUF -> CATn
C   In/out:
C      CATBLK I(256)    AIPS header: Int incarnation
C      CATH   H(*)      AIPS header: Hol incarnation
C      CATR   R(*)      AIPS header: Real incarnation
C      CATD   D(*)      AIPS header: Dubl incarnation
C      BUF    I(284)    integer version
C   The calling program must equivalence CATBLK, CATH, CATR, CATD.
C-----------------------------------------------------------------------
      HOLLERITH CATH(256)
      INTEGER   IDIR, CATBLK(256), BUF(291)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
C
      INTEGER   IC4, IC8, IP, LP, I
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       conversion pointers
      IF (IDIR.LE.1) IC4 = 1
      IF (IDIR.GE.2) IC4 = 3
      IC8 = IC4 + 1
      IP = 1
C                                       initial 21 char strings
      CALL CHAVRT (IDIR, 8, BUF(1), CATH(KHOBJ))
      CALL CHAVRT (IDIR, 8, BUF(5), CATH(KHTEL))
      CALL CHAVRT (IDIR, 8, BUF(9), CATH(KHINS))
      CALL CHAVRT (IDIR, 8, BUF(13), CATH(KHOBS))
      CALL CHAVRT (IDIR, 8, BUF(17), CATH(KHDOB))
      CALL CHAVRT (IDIR, 8, BUF(21), CATH(KHDMP))
      CALL CHAVRT (IDIR, 8, BUF(25), CATH(KHBUN))
      LP = KHPTP - 2
      IP = 29
      DO 10 I = 1,KIPTPN
         LP = LP + 2
         CALL CHAVRT (IDIR, 8, BUF(IP), CATH(LP))
         IP = IP + 4
 10      CONTINUE
      LP = KHCTP - 2
      DO 20 I = 1,KICTPN
         LP = LP + 2
         CALL CHAVRT (IDIR, 8, BUF(IP), CATH(LP))
         IP = IP + 4
 20      CONTINUE
C                                       D   scaling parms, CRVs
      DO 30 I = 1,KICTPN
         CALL REAVRT (IC8, CATD(KDCRV+I-1), BUF(IP))
         IP = IP + 4
 30      CONTINUE
      DO 40 I = 1,KICTPN
         CALL REAVRT (IC4, CATR(KRCIC+I-1), BUF(IP))
         IP = IP + 2
 40      CONTINUE
      DO 50 I = 1,KICTPN
         CALL REAVRT (IC4, CATR(KRCRP+I-1), BUF(IP))
         IP = IP + 2
 50      CONTINUE
      DO 60 I = 1,KICTPN
         CALL REAVRT (IC4, CATR(KRCRT+I-1), BUF(IP))
         IP = IP + 2
 60      CONTINUE
      CALL REAVRT (IC4, CATR(KREPO), BUF(IP))
      IP = IP + 2
      CALL REAVRT (IC4, CATR(KRDMX), BUF(IP))
      IP = IP + 2
      CALL REAVRT (IC4, CATR(KRDMN), BUF(IP))
      IP = IP + 2
      CALL REAVRT (IC4, CATR(KRBLK), BUF(IP))
      IP = IP + 2
C                                       I   # groups
      IF (IDIR.LE.1) THEN
         BUF(IP) = CATBLK(KIGCN)
         BUF(IP+1) = CATBLK(KIPCN)
         BUF(IP+2) = CATBLK(KIDIM)
         CALL COPY (KICTPN, CATBLK(KINAX), BUF(IP+3))
         IP = IP + 3 + KICTPN
         BUF(IP) = CATBLK(KIIMS)
      ELSE
         CATBLK(KIGCN) = BUF(IP)
         CATBLK(KIPCN) = BUF(IP+1)
         CATBLK(KIDIM) = BUF(IP+2)
         CALL COPY (KICTPN, BUF(IP+3), CATBLK(KINAX))
         IP = IP + 3 + KICTPN
         CATBLK(KIIMS) = BUF(IP)
         END IF
      IP = IP + 1
      CALL CHAVRT (IDIR, 20, BUF(IP), CATH(KHIMN))
      IP = IP + 10
C                                       cat -> buf
      IF (IDIR.LE.1) THEN
         BUF(IP) = CATBLK(KIIMU)
         BUF(IP+1) = CATBLK(KINIT)
      ELSE
         CATBLK(KIIMU) = BUF(IP)
         CATBLK(KINIT) = BUF(IP+1)
         END IF
      IP = IP + 2
      CALL REAVRT (IC4, CATR(KRBMJ), BUF(IP))
      CALL REAVRT (IC4, CATR(KRBMN), BUF(IP+2))
      CALL REAVRT (IC4, CATR(KRBPA), BUF(IP+4))
      IP = IP + 6
      IF (IDIR.LE.1) THEN
         BUF(IP) = CATBLK(KITYP)
         BUF(IP+1) = CATBLK(KIALT)
      ELSE
         CATBLK(KITYP) = BUF(IP)
         CATBLK(KIALT) = BUF(IP+1)
         END IF
      IP = IP + 2
C                                       Alt reference vel/freq
      CALL REAVRT (IC8, CATD(KDRST), BUF(IP))
      CALL REAVRT (IC8, CATD(KDARV), BUF(IP+4))
      CALL REAVRT (IC4, CATR(KRARP), BUF(IP+8))
      IP = IP + 10
C                                       Image catalog parms
      CALL REAVRT (IC4, CATH(IRRAN), BUF(IP))
      CALL REAVRT (IC4, CATH(IRRAN+1), BUF(IP+2))
      IP = IP + 4
      IF (IDIR.LE.1) THEN
         BUF(IP) = CATBLK(IIVOL)
         BUF(IP+1) = CATBLK(IICNO)
         CALL COPY (4, CATBLK(IIWIN), BUF(IP+2))
         CALL COPY (5, CATBLK(IIDEP), BUF(IP+6))
         CALL COPY (4, CATBLK(IICOR), BUF(IP+11))
         CALL CHAVRT (IDIR, 2, BUF(IP+15), CATH(IITRA))
         BUF(IP+16) = CATBLK(IIPLT)
         CALL COPY (31, CATBLK(IIOTH), BUF(IP+17))
      ELSE
         CATBLK(IIVOL) = BUF(IP)
         CATBLK(IICNO) = BUF(IP+1)
         CALL COPY (4, BUF(IP+2), CATBLK(IIWIN))
         CALL COPY (5, BUF(IP+6), CATBLK(IIDEP))
         CALL COPY (4, BUF(IP+11), CATBLK(IICOR))
         CALL CHAVRT (IDIR, 2, BUF(IP+15), CATH(IITRA))
         CATBLK(IIPLT) = BUF(IP+16)
         CALL COPY (31, BUF(IP+17), CATBLK(IIOTH))
         END IF
      IP = IP + 48
C
 999  RETURN
      END

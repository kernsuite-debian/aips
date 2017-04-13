      PROGRAM POLCO
C-----------------------------------------------------------------------
C! Removed the bias from polarized intensity maps.
C# Map
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2008-2009
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
C   POLCO corrects the bias in total polarized intensity maps which
C   occurs when the Q and U maps (gaussian statistics) are combined.
C   A maximum likelihood estimator is used.
C     Inputs:
C        USERID               Owner of the image
C        INNAME(3)            Image name (name)
C        INCLASS(2)           Image name (class)
C        INSEQ                Image name (seq. #)
C        INDISK               Disk # of image
C        OUTNAME(3)           Output image name (name)
C        OUTCLASS(2)          Output image name (class)
C        OUTSEQ               Output image name (seq. #)
C        OUTDISK              Disk # of output image
C        PIXSTD               Sigma of noise distribution in Q or U map
C        PCUT                 Blank below P/sigma = PCUT
C        OPCODE               Type of blanking (zeros or magic no.)
C        BADDISK(10)          Disks to avoid for scratch
C        Neil Killeen June 1983.
C        Major revision, April 1986, to output only floating point maps
C        and increase accuracy of correction algorithm.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER DEFNAM*36, OUTNAM*36, INNAM*36, PRGNAM*6, CHTMP*4
      HOLLERITH MAP
      INTEGER   CATIN(256), CATBLK(256), LIN, LOUT, NPARMS,
     *   IERR, IER, INSL, OUTSL, NROWS, NPIX, I, J, L, IROUND, OUTVOL
      REAL   RPARM(28), CATIR(256), CATR(256), BLKVAL, NBLANK,
     *   PDATA(MAXIMG), SIGMA,  BLRAT, POVSIG, DMIN, DMAX
      DOUBLE PRECISION CATID(128), CATD(128)
C
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DBUF.INC'
      INCLUDE 'INCS:DITB.INC'
C
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATIN, CATIR, CATID) , (CATBLK, CATR, CATD)
C
      DATA PRGNAM /'POLCO '/
      DATA LIN, LOUT /17, 18/
C-----------------------------------------------------------------------
C                                       Start up task and get inputs
      NPARMS = 27
      IER = 0
      CALL TSKBEG (PRGNAM, NPARMS, RPARM(2), IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (MSGTXT,1000) IERR
         GO TO 900
 10   RPARM(1) = NLUSER
C                                       Setup namestring
      CALL CHR2H (4, 'MA  ', 1, MAP)
      CALL H2WAWA (RPARM(2), RPARM(5), RPARM(7), MAP, RPARM(8),
     *   RPARM(1), INNAM)
C                                       Open input map
      CALL OPENCF (LIN, INNAM, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,2000) IERR
         GO TO 900
C                                       Get header
 40   CALL GETHDR (LIN, CATIN, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,2500) IERR
         GO TO 900
C                                       Create output namestrings
 50   DEFNAM = INNAM
      CALL H2WAWA (RPARM(9), RPARM(12), RPARM(14), MAP, RPARM(15),
     *   RPARM(1), OUTNAM)
C                                       Create output map header
      DO 60  I = 1,256
         CATBLK(I) = CATIN(I)
 60      CONTINUE
C                                       Fill bad disk array
      DO 65 I = 1, 10
         IBAD(I) = IROUND (RPARM(I+18))
 65      CONTINUE
C                                       Create output map
      CALL MAPCR (DEFNAM, OUTNAM, CATBLK, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,3000) IERR
         GO TO 900
C                                       Open output map
 70   CALL OPENCF (LOUT, OUTNAM, IERR)
      IF (IERR.EQ.0) GO TO 80
         WRITE (MSGTXT,4000) IERR
         GO TO 990
C                                       Get some parameters
 80   INSL = FILTAB(POCAT,6)
      OUTSL = FILTAB(POCAT,7)
      OUTVOL = FILTAB(POVOL,7)
      NPIX = CATIN(KINAX)
      NROWS = CATIN(KINAX+1)
      SIGMA = RPARM(16)
      BLRAT = RPARM(17)
C                                       Min  PCUT = 2-sigma
      WRITE (MSGTXT,4500) BLRAT
      CALL MSGWRT (8)
C                                       Set up blanks counter & extrema
      NBLANK = 0.0
      CALL H2CHR (4, 1, RPARM(18), CHTMP)
      IF (CHTMP.EQ.'ZERO') THEN
         BLKVAL = 0.0
      ELSE
         BLKVAL = INDEF
         END IF
      DMIN = 1.0E30
      DMAX = -1.0
C                                       Loop over rows
      DO 160 J = 1, NROWS
C                                       Read in a row of data
        CALL MAPIO ('READ', LIN, PDATA, IERR)
        IF (IERR.EQ.0) GO TO 120
           WRITE (MSGTXT,5000) J,IERR
           GO TO 900
C                                       Loop over row pixels
 120      DO 140  L = 1,NPIX
             IF (PDATA(L).EQ.INDEF) THEN
                PDATA(L) = BLKVAL
             ELSE
                POVSIG = PDATA(L) / SIGMA
C                                       Make the correction
                IF (POVSIG.GT.BLRAT) THEN
                   CALL MAXLIK (PDATA(L), POVSIG)
                   IF (PDATA(L).NE.INDEF) THEN
                      DMAX = MAX (DMAX, PDATA(L))
                      DMIN = MIN (DMIN, PDATA(L))
                   ELSE
                      PDATA(L) = BLKVAL
                      END IF
C                                       P/sig < min val, pixel blanked
                ELSE
                   PDATA(L) = BLKVAL
                   END IF
                END IF
             IF (PDATA(L).EQ.BLKVAL) NBLANK = NBLANK + 1.0
 140         CONTINUE
C                                       Write output row
        CALL MAPIO ('WRIT', LOUT, PDATA, IERR)
        IF (IERR.EQ.0) GO TO 150
           WRITE (MSGTXT,6000) J,IERR
           GO TO 900
 150       CONTINUE
 160  CONTINUE
C                                       Update header and close maps
      IF (NBLANK.GT.0.5 .AND. BLKVAL.NE.INDEF) DMIN = 0.0
      CATR(KRDMN) = DMIN
      CATR(KRDMX) = DMAX
      IF (BLKVAL.EQ.INDEF .AND. NBLANK.GT.0.5) CATR(KRBLK) = INDEF
      CALL CATIO ('UPDT', OUTVOL, OUTSL, CATBLK, 'REST', WBUFF, IERR)
      CALL FILCLS (LIN)
      CALL FILCLS (LOUT)
C                                       Tell user how many pixels
C                                       blanked
      WRITE (MSGTXT,7000) NBLANK
      CALL MSGWRT (8)
C                                       Add history file
      CALL POLCHI (INNAM, OUTNAM, INSL, OUTSL, SIGMA, BLRAT, IERR)
      IF (IERR.EQ.0) GO TO 180
         WRITE (MSGTXT,9000) IERR
         GO TO 900
 180  CALL CLENUP
      GO TO 990
C                                       Error return
 900  IER = 1
      CALL MSGWRT (8)
      CALL CLENUP
C                                       Normal return
 990  CALL TSKEND (IER)
      STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('TSKBEG:COULD NOT GET INPUTS. IERR = ',I6)
 2000 FORMAT ('OPENCF:COULD NOT OPEN INPUT MAP. IERR = ',I6)
 2500 FORMAT ('GETHDR:COULD NOT READ IN HEADER. IERR = ',I6)
 3000 FORMAT ('MAPCR:COULD NOT CREATE OUTPUT MAP. IERR = ',I6)
 4000 FORMAT ('OPENCF:COULD NOT OPEN OUTPUT MAP. IERR = ',I6)
 4500 FORMAT ('Blanking below ',F6.2,'-sigma')
 5000 FORMAT ('MAPIO:COULD NOT READ LINE # ',I5,'IERR = ',I6)
 6000 FORMAT ('MAPIO:COULD NOT WRITE LINE # ',I5,'IERR = ',I6)
 7000 FORMAT ('There are ', 1PE13.6,' blank pixels in the output image')
 9000 FORMAT ('POLCHI:COULD NOT MAKE HISTORY FILE. IERR = ',I6)
      END
      SUBROUTINE MAXLIK (P, RAT)
C-----------------------------------------------------------------------
C   MAXLIK estimates the intrinsic total polarized intensity from the
C   observed total polarized intensity with the maximum likelihood
C   estimator (see Killeen, Bicknell, and Ekers, ApJ, 1986, 302, 306).
C   The m.l. solution is not analytic. This subroutine uses an
C   asymptotic solution with an empirically determined correction.
C   when RAT >=2.  Otherwise, it uses a Wardle and Kronberg estimation
C   intended mostly to give a better estimate of the noise.
C    Inputs:
C        P               R     On input, P is the observed total
C                              polarized intensity
C        RAT             R     The ratio of the observed total polarized
C                              intensity to the standard deviation of
C                              the (assumed equal) Gaussian
C                              distributions of the Stokes Q or U maps.
C                              It is assumed that RAT>=2.
C    Output:
C        P               R     On output, P is the estimated intrinsic
C                              total polarized intensity.
C-----------------------------------------------------------------------
      REAL   P, RAT, DELTA, C(4), L2, L3, L4, PFAC
      INCLUDE 'INCS:DDCH.INC'
      DATA C /1.283, -12.586, 13.858, -7.686/
C-----------------------------------------------------------------------
      IF (RAT.GE.2.0) THEN
         L2 = LOG10 (RAT)
         L3 = L2 * L2
         L4 = L2 * L3
         PFAC = SQRT (1.0 - (1.0 / (RAT*RAT)))
C
         IF (RAT.GE.5.0) THEN
            DELTA = 0.0
            P = P * PFAC
         ELSE
            DELTA = 10.0**(C(1) + C(2) * L2 + C(3) * L3 + C(4) * L4)
            P = P * (PFAC - DELTA)
            END IF
      ELSE IF (RAT.GT.0.0) THEN
         PFAC = 1.0 - (1.2 / RAT) ** 2
         IF (PFAC.GE.0.0) THEN
            PFAC = SQRT (PFAC)
         ELSE
            PFAC = -SQRT(-PFAC)
            END IF
         P = PFAC * P
      ELSE
         P = FBLANK
         END IF
C
      RETURN
      END
      SUBROUTINE POLCHI (INNAM, OUTNAM, INSL, OUTSL, SIGMA, BLRAT, IERR)
C-----------------------------------------------------------------------
C        POLCHI creates and writes the HI file for the task POLCO
C    Inputs:
C        INNAM(9)         C*36  Input map namestring
C        OUTNAM(9)        C*36  Output map namestring
C        INSL             I     Slot number for input map
C        OUTSL            I     Slot number for output map
C        SIGMA            R     Input sigma on Q or U map
C        BLRAT            R     Blank below P/sigma = BLRAT
C   Outputs:
C        IERR             I     Error return
C                               0-> okay
C                               1-> no good
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6, INNAM*36, OUTNAM*36, HILINE*72,
     *   INAME*12, ICLASS*6, ONAME*12, OCLASS*6, PTYPE*2
      INTEGER   IER, IERR, NHISTF, LHIN, LHOUT, INSL, OUTSL, IVOL, OVOL,
     *   IBUFF1(256), IBUFF2(256), CATBLK(256), ISEQ, OSEQ, USID
      REAL      BLRAT, SIGMA
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      COMMON /MAPHDR/ CATBLK
      DATA NHISTF /2/, LHIN, LHOUT /27, 28/
      DATA PRGNAM /'POLCO '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init HI
      CALL HIINIT (NHISTF)
      IER = 0
      CALL WAWA2A (INNAM, INAME, ICLASS, ISEQ, PTYPE, IVOL, USID)
      CALL WAWA2A (OUTNAM, ONAME, OCLASS, OSEQ, PTYPE, OVOL, USID)
C                                       copy some keywords
      CALL KEYPCP (IVOL, INSL, OVOL, OUTSL, 0, ' ', IERR)
C                                       Copy old to new
      CALL HISCOP (LHIN, LHOUT, IVOL, OVOL, INSL, OUTSL, CATBLK, IBUFF1,
     *   IBUFF2, IERR)
      IF (IERR.LT.3) GO TO 10
         WRITE (MSGTXT,500) IERR
         GO TO 900
C                                       Add INNAME
 10   CALL HENCO1 (PRGNAM, INAME, ICLASS, ISEQ, IVOL, LHOUT, IBUFF2,
     *   IERR)
       IF (IERR.EQ.0) GO TO 20
          WRITE (MSGTXT,1000) IERR
          GO TO 900
C                                       Add OUTNAME
 20   CALL HENCOO (PRGNAM, ONAME, OCLASS, OSEQ, OVOL, LHOUT, IBUFF2,
     *   IERR)
      IF (IERR.EQ.0) GO TO 30
         WRITE (MSGTXT,2000) IERR
         GO TO 900
C                                       Now add sigma
 30   WRITE (HILINE,3000) PRGNAM, SIGMA
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,3500) IERR
         GO TO 900
C                                       Now add blrat
 40   WRITE (HILINE,4000) PRGNAM, BLRAT
      CALL HIADD (LHOUT, HILINE, IBUFF2, IERR)
      IF (IERR.EQ.0) GO TO 950
         WRITE (MSGTXT,5000) IERR
 900  CALL MSGWRT (8)
 950  CALL HICLOS (LHOUT, T, IBUFF2, IERR)
 999  RETURN
C-----------------------------------------------------------------------
  500 FORMAT ('HISCOP: SERIOUS ERROR COPYING OLD HISTORY FILE TO NEW. ',
     *   ' IERR = ',I3)
 1000 FORMAT ('HENCO1: COULD NOT WRITE INNAME ETC. TO HISTORY FILE.',
     *   ' IERR = ',I3)
 2000 FORMAT ('HENCOO: COULD NOT WRITE OUTNAME ETC. TO HISTORY FILE.',
     *   ' IERR = ',I3)
 3000 FORMAT (A6, 1X, '  SIGMA = ', 1PE12.6)
 3500 FORMAT ('HIADD: COULD NOT ADD SIGMA TO HISTORY FILE. IERR = ', I3)
 4000 FORMAT (A6, 1X, '  PCUT = ', 1PE10.3)
 5000 FORMAT ('HIADD: COULD NOT ADD PCUT TO HISTORY FILE. IERR = ', I3)
      END

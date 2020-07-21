      IMPLICIT REAL*8(A-H,O-Z)
      DATA SPEED,ALPHA/10.567D0,60.D0/
      TOL=1.50D0
      R=4100.D0/236000.D0
      ZCOUNT=0.0D0
      H=0.1D-04
      T=0.0D0
      SSN=143687123.D0
      BOTTOM=999999999.D0
      WRITE(0,86) ALPHA
86    FORMAT('1','ALPHA=',F5.2)
      ALPHA=3.141592654D0*ALPHA/180.D0
      U=0.1D-01+(SSN/BOTTOM)*(0.2D-02)
      X=-R*DCOS(ALPHA)-U
      Y=-R*DSIN(ALPHA)
      XD=SPEED*DSIN(ALPHA)
      YD=-SPEED*DCOS(ALPHA)
      WRITE(0,85) XD,YD
85    FORMAT(' ','XD0=',F12.8,5X,'YD0=',F12.8)
      RE=DSQRT((X+U)*(X+U)+Y*Y)
      RM=DSQRT((X+U-1.D0)*(X+U-1.D0)+Y*Y)
      XDD=2.D0*YD+X-(1.D0-U)*(X+U)/(RE*RE*RE)-U*(X-1.D0+U)/(RM*RM*RM)
      YDD=-2.D0*XD+Y-(1.D0-U)*Y/(RE*RE*RE)-U*Y/(RM*RM*RM)
      XOUT=236.D03*X
      YOUT=236.D03*Y
      REOUT=236.D03*RE
      RMOUT=236.D03*RM
      WRITE(0,100)
100   FORMAT(' ',5X,'X',10X,'Y',9X,'RE',9X,'RM',9X,'H',10X,'T',10X,'U')
      WRITE(0,101) XOUT,YOUT,REOUT,RMOUT,H,T,U
101   FORMAT(' ',4F10.0,2X,2(4X,F7.5),4X,F12.10)
50    T=T+0.1D-04
      ZCOUNT=ZCOUNT+1.D0
      XDP=XD+H+XDD
      YDP=YD+H*YDD
      XP=X+H*((XD+XDP)/2.D0)
      YP=Y+H*((YD+YDP)/2.D0)
      REP=DSQRT((XP+U)*(XP+U)+YP*YP)
      RMP=DSQRT((XP+U-1.D0)*(XP+U-1.D0)+YP*YP)
      XDDP=2.0D0*YDP+XP-(1.D0-U)*(XP+U)/(REP*REP*REP)
     1-U*(XP-1.D0+U)/(RMP*RMP*RMP)
      YDDP=-2.D0*XDP+YP-(1.D0-U)*YP/(REP*REP*REP)-U*YP/(RMP*RMP*RMP)
      SAVEXD=XD
      SAVEYD=YD
      XD=XD+H*((XDD+XDDP)/2.0D0)
      YD=YD+H*((YDD+YDDP)/2.0D0)
      X=X+H*((SAVEXD+XD)/2.0D0)
      Y=Y+H*((SAVEYD+YD)/2.0D0)
      RE=DSQRT((X+U)*(X+U)+Y*Y)
      RM=DSQRT((X+U-1.D0)*(X+U-1.D0)+Y*Y)
      XDD=2.0D0*YD+X-(1.D0-U)*(X+U)/(RE*RE*RE)-U*(X-1.D0+U)/(RM*RM*RM)
      YDD=-2.0D0*XD+Y-(1.D0-U)*Y/(RE*RE*RE)-U*Y/(RM*RM*RM)
      XOUT=236.D03*X
      YOUT=236.D03*Y
      RMOUT=236.D03*RM
      REOUT=236.D03*RE
      IF(ZCOUNT.EQ.1000.D0) THEN
      WRITE(0,101) XOUT,YOUT,REOUT,RMOUT,H,T,U
      ZCOUNT=0.0D0
      ENDIF
      IF(T.LE.TOL) GOTO 50
10    CONTINUE
      STOP
      END
      

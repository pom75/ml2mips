/**
 *  fib.java engendre par ml2java 
 */


/**
 *  de'claration de la fonction fib___1
 *    vue comme la classe : MLfun_fib___1
 */ 
class MLfun_fib___1 extends MLfun {

  private static int MAX = 1;

  MLfun_fib___1() {super();}

  MLfun_fib___1(int n) {super(n);}

  public MLvalue invoke(MLvalue MLparam){
    if (MLcounter == (MAX-1)) {
      return invoke_real(MLparam);
    }
    else {
      MLfun_fib___1 l = new MLfun_fib___1(MLcounter+1);l.MLaddenv(MLenv,MLparam); return l;
    }
  }


  MLvalue invoke_real(MLvalue x___2) {

    { 
      MLvalue T___3;
      { 
        MLvalue T___4;
        MLvalue T___5;
        T___4=x___2;
        T___5=new MLint(2);
        T___3=MLruntime.MLltint( (MLint )T___4,(MLint )T___5);
      }
      if (((MLbool)T___3).MLaccess())
        { 
          MLvalue T___6;
          T___6=new MLint(1);
          return T___6;
        }
      else
        { 
          MLvalue T___7;
          { 
            MLvalue T___8;
            MLvalue T___13;
            { 
              MLvalue T___9;
              MLvalue T___10;
              T___9=fib.fib___1;
              { 
                MLvalue T___11;
                MLvalue T___12;
                T___11=x___2;
                T___12=new MLint(1);
                T___10=MLruntime.MLsubint( (MLint )T___11,(MLint )T___12);
              }
              T___8=((MLfun)T___9).invoke(T___10);
            }
            { 
              MLvalue T___14;
              MLvalue T___15;
              T___14=fib.fib___1;
              { 
                MLvalue T___16;
                MLvalue T___17;
                T___16=x___2;
                T___17=new MLint(2);
                T___15=MLruntime.MLsubint( (MLint )T___16,(MLint )T___17);
              }
              T___13=((MLfun)T___14).invoke(T___15);
            }
            T___7=MLruntime.MLaddint( (MLint )T___8,(MLint )T___13);
          }
          return T___7;
        }
    }
  }

}
// fin de la classe MLfun_fib___1
/**
 * 
 */
class fib {

  static MLvalue fib___1= new MLfun_fib___1(1);
  static MLvalue value___18;

public static void main(String []args) {

{ 
  MLvalue T___19;
  MLvalue T___20;
  T___19=fib.fib___1;
  T___20=new MLint(5);
  value___18=((MLfun)T___19).invoke(T___20);
}
{ 
  MLvalue bidon___21;
  bidon___21=MLruntime.MLlrp;
  bidon___21=MLruntime.MLprint( (MLvalue )value___18);
}
}}

// fin du fichier fib.java

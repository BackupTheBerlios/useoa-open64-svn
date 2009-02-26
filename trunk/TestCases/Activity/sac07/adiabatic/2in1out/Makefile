AD_COMP = sun4-SC1.0

g_aifl: g_aifl.o g_dnsty.o g_hdel.o g_hmx.o g_hvap.o g_iflash.o jdriver.o g_pvap.o g_tbtd.o g_tset.o g_vmgas.o g_vmliq.o g_wtmol.o
	f77 -o g_aifl g_aifl.o g_dnsty.o g_hdel.o g_hmx.o g_hvap.o g_iflash.o jdriver.o g_pvap.o g_tbtd.o g_tset.o g_vmgas.o g_vmliq.o g_wtmol.o $(AD_LIB)/lib/ReqADIntrinsics-$(AD_COMP).o $(AD_LIB)/lib/libADIntrinsics-$(AD_COMP).a

process: ../aifl2.adf
	cd ..; Adifor2.0 AD_SCRIPT=aifl2.adf
 
clean:
	rm -f *.A *.aux *.o *~

reallyclean: clean
	rm -f g_* out.*

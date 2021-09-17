all:
#	service
	rm -rf ebin/* *.lgh lgh;
	erlc -I ../interfaces -o ebin src/*.erl;
	rm -rf src/*.beam *.beam  test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log catalog;
	rm -rf *_pod*;
	echo Done
unit_test:
	rm -rf ebin/* src/*.beam *.beam test_src/*.beam test_ebin;
	rm -rf  *~ */*~  erl_cra*;
	rm -rf *_specs *_config *.log *_pod*;
	mkdir test_ebin;
#	interface
	erlc -I ../interfaces -o ebin ../interfaces/*.erl;
#	support
	erlc -I ../interfaces -o ebin ../kube_support/src/*.erl;
#	etcd
	cp ../applications/etcd/src/*.app ebin;
	erlc -I ../interfaces -o ebin ../kube_dbase/src/*.erl;
	erlc -I ../interfaces -o ebin ../applications/etcd/src/*.erl;
#	kubelet
	cp ../applications/kubelet/src/*.app ebin;
	erlc -I ../interfaces -o ebin ../applications/kubelet/src/*.erl;
	erlc -I ../interfaces -o ebin src/*.erl;
#	test application
	cp test_src/*.app test_ebin;
	erlc -I ../interfaces -o test_ebin test_src/*.erl;
	erl -pa ebin -pa test_ebin\
		-setcookie lgh_cookie\
		-sname test_node\
		-unit_test monitor_node kubelet_lgh\
		-unit_test cluster_id lgh\
		-unit_test cookie lgh_cookie\
		-run unit_test start_test test_src/test.config

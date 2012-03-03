#!/bin/bash

erl -pa ebin -config sys_config.config -run dummy_smsc start -smsc system_id TEST -smsc password TEST -smsc log log

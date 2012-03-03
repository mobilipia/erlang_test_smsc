#!/bin/bash

erl -pa ebin -run dummy_smsc start -smsc system_id TEST -smsc password TEST

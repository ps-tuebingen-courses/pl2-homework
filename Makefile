build:
ifeq ($(num),)
	@echo Please provide argument num
	@echo make build num=xx
else
	./build.sh $(num) $(deadline)
endif

clean:
	find -name "*.aux" -delete 
	find -name "*.log" -delete
	find -name "*.pdf" -delete


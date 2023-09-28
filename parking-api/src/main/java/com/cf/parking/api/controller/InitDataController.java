package com.cf.parking.api.controller;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.cf.parking.dao.po.EmployeePO;
import com.cf.parking.dao.po.LotteryApplyRecordPO;
import com.cf.parking.dao.po.LotteryBatchPO;
import com.cf.parking.dao.po.UserPO;
import com.cf.parking.dao.po.UserProfilePO;
import com.cf.parking.dao.po.UserVerifyPO;
import com.cf.parking.services.service.EmployeeService;
import com.cf.parking.services.service.LotteryApplyRecordService;
import com.cf.parking.services.service.LotteryBatchService;
import com.cf.parking.services.service.UserProfileService;
import com.cf.parking.services.service.UserService;
import com.cf.parking.services.service.UserVerifyService;
import com.cf.support.bean.IdWorker;
import com.cf.support.result.Result;
import io.swagger.annotations.Api;



@RestController
@RequestMapping("/init")
@Api(tags = "初始化数据接口")
public class InitDataController {

	private static List<String> deptList = Arrays.asList("10001001",
			"10001002",
			"10001013",
			"10001014",
			"10001015",
			"10001016",
			"10001017",
			"10001018",
			"10001019",
			"10001020");
	
	@Resource
	private UserService userService;
	
	@Resource
	private EmployeeService employeeService;
	
	@Resource
	private UserProfileService userProfileService;
	
	@Resource
	private UserVerifyService userVerifyService;
	
	@Resource
	private IdWorker idWorker;
	
	@Resource
	private LotteryBatchService batchService;
	
	@Resource
	private LotteryApplyRecordService lotteryApplyRecordService;
	
	
	
	@GetMapping("/profile")
	public Result profile() {
		
		List<UserPO> userList =  userService.list();
		userList.forEach(user -> {
			for (int j = 0 ; j< 2; j++) {
				
				EmployeePO employee = employeeService.queryEmployeeByOpenId(user.getOpenId());
				
				if (employee == null) {
					continue;
				}
				
				UserVerifyPO verify = new UserVerifyPO();
				verify.setCreateTm(new Date());
				verify.setId(idWorker.nextId());
				verify.setPlateNo(user.getOpenId()+ j);
				verify.setState(1);
				verify.setUserId(user.getUserId());
				verify.setDrivingLicenseImg("");
				verify.setDrivingPermitImg("");
				verify.setVehicleImg("");
				verify.setUserName(employee.getName());
				verify.setUpdateTm(new Date());
				userVerifyService.save(verify);
			}
		});
		
		return Result.buildSuccessResult();
	}
	
	
	
	@GetMapping("/user")
	public Result init() {
		
		int size = deptList.size();
		
		for (int i = 0; i< 2000; i++) {
			UserPO user = initUser(i);
			userService.save(user);
			
			EmployeePO employee = new EmployeePO();
			employee.setDeptCode(deptList.get(i%size));
			employee.setEmplNo(user.getOpenId());
			employee.setName("CFDL00" + i );
			employee.setState(0);
			employee.setCreateTm(new Date());
			employeeService.save(employee);
			
			UserProfilePO profile = new UserProfilePO();
			profile.setCreateTm(new Date());
			profile.setUserId(user.getUserId());
			profile.setName(employee.getName());
			profile.setJobNumber(user.getOpenId());
			userProfileService.save(profile);
			
			for (int j = 0 ; j< 2; j++) {
				UserVerifyPO verify = new UserVerifyPO();
				verify.setCreateTm(new Date());
				verify.setId(idWorker.nextId());
				verify.setPlateNo(user.getOpenId()+ j);
				verify.setState(1);
				verify.setUserId(user.getUserId());
				verify.setUserName(employee.getName());
				verify.setUpdateTm(new Date());
				verify.setDrivingLicenseImg("");
				verify.setDrivingPermitImg("");
				verify.setVehicleImg("");
				userVerifyService.save(verify);
			}
			
		}
		
		return Result.buildSuccessResult();
	}
	
	
	/**
	 * 生成报名数据
	 * @return
	 */
	@GetMapping("/apply")
	public Result apply(long batchId) {
		LotteryBatchPO batch = batchService.getById(batchId);
		List<UserPO> userList =  userService.list();
		userList.forEach(user -> {
			EmployeePO employee = employeeService.queryEmployeeByOpenId(user.getOpenId());
			
			if (employee == null) {
				return;
			}
			
			LotteryApplyRecordPO record = new LotteryApplyRecordPO();
			record.setApplyState("1");
			record.setResult("-1");
			record.setCreateTm(new Date());
			record.setUpdateTm(new Date());
			record.setBatchId(batch.getId());
			record.setBatchNum(batch.getBatchNum());
			record.setId(idWorker.nextId());
			record.setJobNumber(user.getOpenId());
			record.setUserId(user.getUserId());
			record.setUserName(employee.getName());
			record.setValidEndDate(batch.getValidEndDate());
			record.setValidStartDate(batch.getValidStartDate());
			lotteryApplyRecordService.save(record);
		});
		
		return Result.buildSuccessResult();
	}
	
	private UserPO initUser(int num ) {
		UserPO user = new UserPO();
		user.setState(1);
		user.setOpenId("CFDL00" + num);
		user.setUserId(idWorker.nextId());
		return user;
	}
}

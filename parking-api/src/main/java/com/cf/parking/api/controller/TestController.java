package com.cf.parking.api.controller;

import com.cf.parking.api.request.BlackListBatchAddReq;
import com.cf.parking.facade.constant.FeignUrlConstant;
import com.cf.parking.facade.dto.*;
import com.cf.parking.facade.facade.BlackListFacade;
import com.cf.parking.facade.facade.CrossRecordsFacade;
import com.cf.parking.facade.facade.DingNoticeRecordFacade;
import com.cf.parking.facade.facade.ScheduleDataFacade;
import com.cf.parking.services.integration.GatewayHikvisionFeign;
import com.cf.parking.services.integration.ParkInvokeService;
import com.cf.parking.services.job.parking.ParkingSpaceTask;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import com.cf.support.utils.BeanConvertorUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;


/**
 * 用于给测试使用的接口，不提供生产环境使用
 *
 * @Classname TestController
 * @Date 2022/11/15 16:09
 * @Created by csy
 */
@Slf4j
@RestController
@RequestMapping("/test")
@Api(tags = "测试接口")
public class TestController {
	@Resource
	private ParkingSpaceTask parkingSpaceTask;
	@Resource
	private GatewayHikvisionFeign gatewayHikvisionFeign;
	@Resource
	private DingNoticeRecordFacade dingNoticeRecordFacade;
	@Resource
	private BlackListFacade blackListFacade;
	@Resource
	private ScheduleDataFacade scheduleDataFacade;
	@Resource
	private CrossRecordsFacade crossRecordsFacade;
	
	@Resource
	private ParkInvokeService parkInvokeService ;

	/**
	 * 执行获取车位信息
	 *
	 * @return
	 */
	@GetMapping("/sp")
	public Result getParkingUserSpace() {
		parkingSpaceTask.getUserSpace();
		return Result.buildSuccessResult();
	}

	/**
	 * 手动添加黑名单
	 *
	 * @param blackListBatchAddReq
	 * @return
	 */
	@PostMapping("/blackAdd")
	public Result addBlackList(@RequestBody BlackListBatchAddReq blackListBatchAddReq) {
		// TODO:添加黑名单加校验
		List<BlackListBatchAdditionDTO> blackListBatchAdditionDTOS = BeanConvertorUtils.copyList(blackListBatchAddReq.getList(), BlackListBatchAdditionDTO.class);
		blackListFacade.blackListAddition(blackListBatchAdditionDTOS);
		return Result.buildSuccessResult();
	}

	/**
	 * 执行获取车位信息
	 *
	 * @return
	 */
	@GetMapping("/test")
	public Result test() {
		CarInRecordQueryDTO carInRecordQueryDTO = new CarInRecordQueryDTO().setParkTime(String.valueOf(24)).setPageNo(1).setPageSize(1000);
		HikvisionResult<PageResponse<CarInRecordDTO>> result = gatewayHikvisionFeign.tempCarInRecords("artemis/api/pms/v1/tempCarInRecords/page", carInRecordQueryDTO);
		PageResponse pageResponse = result.getData();
		List<CarInRecordDTO> carInRecordDTOList = pageResponse.getList();
		return Result.buildSuccessResult();
	}

	/**
	 * 钉钉通知测试接口
	 *
	 * @return
	 */
	@PostMapping("/ding")
	@ApiOperation(value = "钉钉通知测试接口", notes = "钉钉通知测试接口")
	public Result dingNotify() {
		String msg = "Dear XXX(CFDL00004),已超24小时没有您爱车的出入记录，提醒3次将拉入黑名单！";
		List<DingNoticeRecordDTO> dingNoticeRecordDTOList = Arrays.asList(new DingNoticeRecordDTO().setMessage(msg).setJobNumber("CFDL15244"));
		dingNoticeRecordFacade.dingNotify(dingNoticeRecordDTOList);
		return Result.buildSuccessResult();
	}
	/**
	 * 钉钉通知测试接口
	 *
	 * @return
	 */
	@PostMapping("/notParking")
	@ApiOperation(value = "不停车测试接口", notes = "不停车测试接口")
	public Result notParking() {
		parkingSpaceTask.notParked();
		return Result.buildSuccessResult();
	}

	@PostMapping("/zombieVehicle")
	@ApiOperation(value = "僵尸车测试接口", notes = "僵尸车测试接口")
	public Result zombieVehicle() {
		parkingSpaceTask.zombieVehicle();
		return Result.buildSuccessResult();
	}

	/**
	 * 获取当季排班记录
	 */
	@PostMapping("/attendance")
	@ApiOperation(value = "获取当季排班记录", notes = "获取当季排班记录")
	public Result getGaiaAttendance() {
		scheduleDataFacade.getGaiaAttendance();
		return Result.buildSuccessResult();
	}

	/**
	 * 获取过车记录
	 */
	@PostMapping("/cross")
	@ApiOperation(value = "获取过车记录", notes = "获取过车记录")
	public Result getCrossRecords() {
		Integer pageNo = 1;
		Integer total = crossRecordsFacade.saveCrossRecords(pageNo);
		for (int i = 2; i <= total / 1000 + 1; i++) {
			crossRecordsFacade.saveCrossRecords(i);
		}
		return Result.buildSuccessResult();
	}

	@GetMapping("/userSpace")
	@ApiOperation(value = "获取车位", notes = "获取车位")
	public Result getUserSpace() {
		parkingSpaceTask.getUserSpace();
		return Result.buildSuccessResult();
	}

	@PostMapping("/setCarCharge")
	@ApiOperation(value = "车辆充值", notes = "车辆充值")
	public Result setCarCharge() {
		CarChargeDTO carChargeDTO = new CarChargeDTO().setParkSyscode("6dc5132a6a7046c09ffd7be54d27ea49").setPlateNo("浙AF72886")
				.setStartTime("2023-09-05").setEndTime("2023-09-06");
		HikvisionResult<String> hikvisionResult = gatewayHikvisionFeign.setCarCharge(FeignUrlConstant.CAR_CHARGE_SET_URL, carChargeDTO);
		log.info("setCarCharge={}", hikvisionResult);
		return Result.buildSuccessResult(hikvisionResult.getMsg());
	}

	@PostMapping("/delCarCharge")
	@ApiOperation(value = "取消车辆包期", notes = "取消车辆包期")
	public Result delCarCharge() {
		CarChargeDelDTO carChargeDelDTO = new CarChargeDelDTO().setParkSyscode("6dc5132a6a7046c09ffd7be54d27ea49").setPlateNo("浙AF72886");
		HikvisionResult<String> hikvisionResult = gatewayHikvisionFeign.delCarCharge(FeignUrlConstant.CAR_CHARGE_DEL_URL, carChargeDelDTO);
		log.info("delCarCharge={}", hikvisionResult);
		return Result.buildSuccessResult(hikvisionResult.getMsg());
	}

	@PostMapping("/getParkList")
	@ApiOperation(value = "获取停车库列表", notes = "获取停车库列表")
	public Result getParkList() {
		GetParkListDTO getParkListDTO = new GetParkListDTO();
		//getParkListDTO.setParkIndexCodes("6dc5132a6a7046c09ffd7be54d27ea49");
		HikvisionResult<List<ParkListDTO>> hikvisionResult = gatewayHikvisionFeign.getParkList(FeignUrlConstant.GET_PARK_URL, getParkListDTO);
		log.info("getParkList={}", hikvisionResult);
		return Result.buildSuccessResult(hikvisionResult.getData());
	}

	@PostMapping("/remainSpaceNum")
	@ApiOperation(value = "查询停车库剩余车位数", notes = "查询停车库剩余车位数")
	public Result remainSpaceNum() {
		HikvisionResult<List<SpaceNumDTO>> hikvisionResult = gatewayHikvisionFeign.remainSpaceNum(FeignUrlConstant.SPACE_NUM_URL, new ParkSyscodeDTO());
		log.info("remainSpaceNum={}", hikvisionResult);
		return Result.buildSuccessResult(hikvisionResult.getData());
	}
	
	
	@PostMapping("/carAdd")
	@ApiOperation(value = "添加车位", notes = "添加车位")
	public Result addCar() {
		HikvisionResult<List<SpaceNumDTO>> hikvisionResult = gatewayHikvisionFeign.remainSpaceNum(FeignUrlConstant.SPACE_NUM_URL, new ParkSyscodeDTO());
		log.info("remainSpaceNum={}", hikvisionResult);
		return Result.buildSuccessResult(hikvisionResult.getData());
	}
	
}
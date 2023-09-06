package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.ParkingSpaceTransferRecordPageReq;
import com.cf.parking.api.response.ParkingSpaceTransferRecordPageRsp;
import com.cf.parking.dao.po.ParkingSpaceTransferRecordPO;
import com.cf.parking.facade.facade.ParkingSpaceTransferRecordFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 车位转赠记录Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@RestController
@RequestMapping("/parkingSpace/transferRecord")
public class ParkingSpaceTransferRecordController
{
    @Resource
    private ParkingSpaceTransferRecordFacade parkingSpaceTransferRecordFacade;

    /**
     * 查询车位转赠记录列表
     */
    @PostMapping("/list")
    public Result<PageResponse<ParkingSpaceTransferRecordPageRsp>> list(@RequestBody ParkingSpaceTransferRecordPageReq param)
    {
        return null;
    }


    /**
     * 获取车位转赠记录详细信息
     */
    @PostMapping( "/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增车位转赠记录
     */
    @PostMapping("/2")
    public Result add(@RequestBody ParkingSpaceTransferRecordPO parkingSpaceTransferRecordPO)
    {
        return null;
    }

    /**
     * 修改车位转赠记录
     */
    @PostMapping("/3")
    public Result edit(@RequestBody ParkingSpaceTransferRecordPO parkingSpaceTransferRecordPO)
    {
        return null;
    }

    /**
     * 删除车位转赠记录
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}

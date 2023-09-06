package com.cf.parking.api.controller;

import javax.annotation.Resource;

import com.cf.parking.api.request.ParkingLotPageReq;
import com.cf.parking.api.response.ParkingLotPageRsp;
import com.cf.parking.dao.po.ParkingLotPO;
import com.cf.parking.facade.facade.ParkingLotFacade;
import com.cf.support.result.PageResponse;
import com.cf.support.result.Result;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 停车场主Controller
 * 
 * @author
 * @date 2023-09-05
 */
@Slf4j
@RestController
@RequestMapping("/parking/lot")
public class ParkingLotController
{
    @Resource
    private ParkingLotFacade parkingLotFacade;

    /**
     * 查询停车场主列表
     */
    @PostMapping("/list")
    public Result<PageResponse<ParkingLotPageRsp>> list(@RequestBody ParkingLotPageReq param)
    {

        return null;
    }


    /**
     * 获取停车场主详细信息
     */
    @PostMapping("/1")
    public Result getInfo(@RequestBody Long id)
    {
        return null;
    }

    /**
     * 新增停车场主
     */
    @PostMapping("/2")
    public Result add(@RequestBody ParkingLotPO parkingLotPO)
    {
        return null;
    }

    /**
     * 修改停车场主
     */
    @PostMapping("/3")
    public Result edit(@RequestBody ParkingLotPO parkingLotPO)
    {
        return null;
    }

    /**
     * 删除停车场主
     */
	@PostMapping("/4")
    public Result remove(@RequestBody Long[] ids)
    {
        return null;
    }
}

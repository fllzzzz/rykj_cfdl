package com.cf.parking.api.response;

import com.cf.parking.facade.bo.ParkingLotBO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * 停车场主对象
 * @author
 * @date 2023/09/05
 */
@Data
@ApiModel(description = "停车场查询结果")
public class ParkingLotRsp {

    /** id */
    @ApiModelProperty(value = "id")
    private Long id;

    /** parentId */
    @ApiModelProperty(value = "parentId")
    private Long parentId;

    /** 区域 */
    @ApiModelProperty(value = "区域")
    private String region;

    /** 区域编号 */
    @ApiModelProperty(value = "区域编号")
    private String regionCode;

    /** 车位数量 */
    @ApiModelProperty(value = "车位数量")
    private Long amount;

    /** 类型(0：可摇号，1：不可摇号) */
    @ApiModelProperty(value = "类型")
    private String type;

    /** 创建时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date createTm;

    /** 更新时间 */
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date updateTm;

    /** 备注 */
    @ApiModelProperty(value = "备注")
    private String remark;

    /**子记录*/
    @ApiModelProperty(value = "子记录")
    private List<ParkingLotRsp> children;

}
